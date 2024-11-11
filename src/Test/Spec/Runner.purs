module Test.Spec.Runner
  ( Reporter
  , TestEvents
  , evalSpecT
  , module Test.Spec.Config
  , run
  , runSpec
  , runSpec'
  , runSpecPure
  , runSpecPure'
  , runSpecT
  )
  where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse, parallel, sequential)
import Data.Array (concat, fold, groupBy)
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
import Test.Spec.Tree (Path, annotatedWithPaths, countTests, isAllParallelizable)

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
  r <- loop keepRunningVar $ annotatedWithPaths $ filteredTests tests
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
        Leaf (path /\ name) (Just (Item item)) -> do
          if keepRunning then do
            yield $ Event.Test (if isParallelizable then Parallel else Sequential) (path /\ name)
            res <- executeExample item.example
            case res of
              Failure _ | config.failFast -> liftEffect $ Ref.write false keepRunningVar
              _ -> pure unit
            yield $ Event.TestEnd (path /\ name) res
            pure [ Leaf name $ Just res ]
          else
            pure [ Leaf name Nothing ]

        Leaf (path /\ name) Nothing -> do
          when keepRunning $
            yield $ Event.Pending (path /\ name)
          pure [ Leaf name Nothing ]

        Node (Right cleanup) xs ->
          loop keepRunningVar xs <* lift (cleanup unit)

        Node (Left (path /\ name)) xs -> do
          when keepRunning $
            yield $ Event.Suite (if isParallelizable then Parallel else Sequential) (path /\ name)
          res <- loop keepRunningVar xs
          when keepRunning $
            yield $ Event.SuiteEnd (path /\ name)
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

-- | Evaluates the spec tree and returns an action, which, when executed, will
-- | run the tests and return the results, which are also reported using
-- | specified Reporters, if any.
evalSpecT
  :: forall m
   . Functor m
  => Config
  -> Array Reporter
  -> SpecT Aff Unit m Unit
  -> m (Aff (Array (Tree String Void Result)))
evalSpecT config reporters spec = _run config spec <#> \runner -> do
  let
    events = foldl (>->) runner reporters
    reportedEvents = P.runEffect $ events //> \_ -> pure unit
  if config.exit
    then do
      liftEffect $ Console.write $ styled Style.yellow $ fold
        [ "WARNING: The use of `runSpec` or `runSpecT` under NodeJS is deprecated "
        , "and will be removed in the next major release. "
        , "Please migrate to `runSpecAndExitProcess` from the 'spec-node' package."
        ]
      try reportedEvents >>= case _ of
        Left err -> do
          liftEffect $ Console.write $ styled Style.red (show err <> "\n")
          liftEffect $ exit 1
          throwError err
        Right results -> liftEffect do
          let code = if successful results then 0 else 1
          exit code
          pure results
    else
      reportedEvents

runSpecPure :: Array Reporter -> Spec Unit -> Aff Unit
runSpecPure reporters spec = runSpecPure' defaultConfig reporters spec

runSpecPure' :: Config -> Array Reporter -> Spec Unit -> Aff Unit
runSpecPure' config reporters spec = void $ un Identity $ evalSpecT config reporters spec

runSpecT
  :: forall m
   . Functor m
  => Warn (Text "`runSpecT` is deprecated. Use `runSpecAndExitProcess` from the 'spec-node' package instead.")
  => Config
  -> Array Reporter
  -> SpecT Aff Unit m Unit
  -> m (Aff (Array (Tree String Void Result)))
runSpecT = evalSpecT

-- | Run the spec with the default config
run
  :: Warn (Text "`run` is deprecated. Use runSpecAndExitProcess from the 'spec-node' package instead.")
  => Array Reporter
  -> Spec Unit
  -> Aff Unit
run = runSpecPure' defaultConfig

-- | Run the spec with the default config
runSpec
  :: Warn (Text "`runSpec` is deprecated. Use runSpecAndExitProcess from the 'spec-node' package instead.")
  => Array Reporter
  -> Spec Unit
  -> Aff Unit
runSpec = runSpecPure

runSpec'
  :: Warn (Text "`runSpec'` is deprecated. Use runSpecAndExitProcess from the 'spec-node' package instead.")
  => Config
  -> Array Reporter
  -> Spec Unit
  -> Aff Unit
runSpec' = runSpecPure'
