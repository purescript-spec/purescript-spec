module Test.Spec.Runner
  ( Reporter
  , TestEvents
  , module Test.Spec.Config
  , run
  , runSpec
  , runSpec'
  , runSpecT
  )
  where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse, parallel, sequential)
import Data.Array (concat, groupBy)
import Data.Array.NonEmpty as NEA
import Data.DateTime.Instant (diff)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (class Traversable, for)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, forkAff, joinFiber, makeAff, throwError, try)
import Effect.Aff.AVar as AV
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)
import Effect.Ref as Ref
import Pipes (yield, (>->))
import Pipes.Core (Producer, Pipe, (//>))
import Pipes.Core (runEffect, runEffectRec) as P
import Prim.TypeError (class Warn, Text)
import Test.Spec (Item(..), Spec, SpecT, SpecTree, Tree(..), collect)
import Test.Spec.Config (Config, TreeFilter(..), defaultConfig)
import Test.Spec.Console as Console
import Test.Spec.Result (Result(..))
import Test.Spec.Runner.Event (Event, Execution(..))
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed (speedOf)
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Summary (successful)
import Test.Spec.Tree (Path, annotateWithPaths, countTests, isAllParallelizable)

foreign import exit :: Int -> Effect Unit

makeTimeout
  :: Milliseconds
  -> Aff Unit
makeTimeout ms@(Milliseconds ms') = do
  delay ms
  makeAff \cb -> mempty <$ do
    cb <<< Left $ error $ "test timed out after " <> show (Int.round ms') <> "ms"

timeout
  :: Milliseconds
  -> Aff Unit
  -> Aff Unit
timeout time t = do
  sequential (parallel (try (makeTimeout time)) <|> parallel (try t))
    >>= either throwError pure

type TestWithPath r = {test :: SpecTree Aff Unit, path :: Path | r}

-- Run the given spec as `Producer` in the underlying `Aff` monad.
-- This producer has two responsibilities:
--      1) emit events for key moments in the runner's lifecycle
--      2) collect the tst output into an array of results
-- This allows downstream consumers to report about the tests even before the
-- prodocer has completed and still benefit from the array of results the way
-- the runner sees it.
_run
  :: forall m
   . Functor m
  => Config
  -> SpecT Aff Unit m Unit
  -> m TestEvents
_run config = collect >>> map \tests -> do
  yield (Event.Start (countTests tests))
  keepRunningVar <- liftEffect $ Ref.new true
  r <- loop keepRunningVar $ annotateWithPaths $ filteredTests tests
  yield (Event.End r)
  pure r
  where
    filteredTests tests = case config.filterTree of
      TreeFilter f -> f tests

    loop :: _ -> Array _ -> TestEvents
    loop keepRunningVar tests = do
      let groups =
            tests
            <#> (\test -> { isParallelizable: isAllParallelizable test, test })
            # groupBy \a b -> a.isParallelizable == b.isParallelizable

      concat <$>
        for groups \g ->
          if (NEA.head g).isParallelizable
            then concat <$> mergeProducers (runItem keepRunningVar <$> NEA.toArray g)
            else concat <$> for (NEA.toArray g) (runItem keepRunningVar)

    runItem :: _ -> { isParallelizable :: Boolean, test :: _ } -> TestEvents
    runItem keepRunningVar { test, isParallelizable } = do
      keepRunning <- liftEffect $ Ref.read keepRunningVar

      case test of
        Leaf (name /\ path) (Just (Item item)) -> do
          if keepRunning then do
            yield $ Event.Test (if isParallelizable then Parallel else Sequential) path name
            res <- executeExample item.example
            case res of
              Failure _ | config.failFast -> liftEffect $ Ref.write false keepRunningVar
              _ -> pure unit
            yield $ Event.TestEnd path name res
            pure [ Leaf name $ Just res ]
          else
            pure [ Leaf name Nothing ]

        Leaf (name /\ path) Nothing -> do
          when keepRunning $
            yield $ Event.Pending path name
          pure [ Leaf name Nothing ]

        Node (Right cleanup) xs ->
          loop keepRunningVar xs <* lift (cleanup unit)

        Node (Left (name /\ path)) xs -> do
          when keepRunning $
            yield $ Event.Suite (if isParallelizable then Parallel else Sequential) path name
          res <- loop keepRunningVar xs
          when keepRunning $
            yield $ Event.SuiteEnd path
          pure [ Node (Left name) res ]

    executeExample f = lift do
      start <- liftEffect now
      let wrap = maybe identity timeout config.timeout
      e <- attempt $ wrap $ f \a -> a unit
      end <- liftEffect now
      let duration = end `diff` start
      pure $ either Failure (const $ Success (speedOf config.slow duration) duration) e


-- https://github.com/felixSchl/purescript-pipes/issues/16
mergeProducers :: forall t o a. Traversable t => t (Producer o Aff a) -> Producer o Aff (t a)
mergeProducers ps = do
  var <- lift AV.empty

  fib <- lift $ forkAff do
    let consumer i = lift (AV.put i var) *> pure unit
    x <- parTraverse (\p -> P.runEffectRec $ p //> consumer) ps
    AV.kill (error "finished") var
    pure x

  let
    loop = do
      res <- lift $ try (AV.take var)
      case res of
        Left _ -> lift $ joinFiber fib
        Right e -> do
          yield e
          loop
  loop

type TestEvents = Producer Event Aff (Array (Tree String Void Result))

type Reporter = Pipe Event Event Aff (Array (Tree String Void Result))

-- | Run the spec with `config`, returning the results, which
-- | are also reported using specified Reporters, if any.
-- | If configured as such, `exit` the program upon completion
-- | with appropriate exit code.
runSpecT
  :: forall m
   . Functor m
  => Config
  -> Array Reporter
  -> SpecT Aff Unit m Unit
  -> m (Aff (Array (Tree String Void Result)))
runSpecT config reporters spec = _run config spec <#> \runner -> do
  let
    events = foldl (>->) runner reporters
    reportedEvents = P.runEffect $ events //> \_ -> pure unit
  if config.exit
    then try reportedEvents >>= case _ of
      Left err -> do
        liftEffect $ Console.write $ styled Style.red (show err <> "\n")
        liftEffect $ exit 1
        throwError err
      Right results -> liftEffect do
        let code = if successful results then 0 else 1
        exit code
        pure results
    else reportedEvents

-- | Run the spec with the default config
run
  :: Warn (Text "`Test.Spec.Runner.run` is Deprecated use runSpec instead")
  => Array Reporter
  -> Spec Unit
  -> Aff Unit
run = runSpec' defaultConfig

-- | Run the spec with the default config
runSpec
  :: Array Reporter
  -> Spec Unit
  -> Aff Unit
runSpec reporters spec = runSpec' defaultConfig reporters spec

runSpec'
  :: Config
  -> Array Reporter
  -> Spec Unit
  -> Aff Unit
runSpec' config reporters spec = void $ un Identity $ runSpecT config reporters spec
