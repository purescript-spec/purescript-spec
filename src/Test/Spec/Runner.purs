module Test.Spec.Runner
  ( run
  , run'
  , runSpec
  , runSpec'
  , defaultConfig
  , timeout
  , Config
  , TestEvents
  , Reporter
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (execWriterT)
import Control.Parallel (parTraverse, parallel, sequential)
import Data.Array (groupBy, mapWithIndex)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (class Traversable, for)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, forkAff, joinFiber, makeAff, throwError, try)
import Effect.Aff.AVar as AV
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Pipes ((>->), yield)
import Pipes.Core (Pipe, Producer, (//>))
import Pipes.Core (runEffectRec) as P
import Test.Spec (Item(..), Spec, SpecM, SpecTree, Tree(..))
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Console (logWriter, tellLn)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner.Event (Event, Execution(..))
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed (speedOf)
import Test.Spec.Summary (successful)
import Test.Spec.Tree (Path, PathItem(..), countTests, discardUnfocused, isAllParallelizable)

foreign import exit :: Int -> Effect Unit

foreign import dateNow :: Effect Int

type Config =
  { slow :: Int
  , timeout :: Maybe Int
  , exit :: Boolean
  }

defaultConfig :: Config
defaultConfig =
  { slow: 75
  , timeout: Just 2000
  , exit: true
  }

makeTimeout
  :: Int
  -> Aff Unit
makeTimeout time = do
  delay (Milliseconds $ toNumber time)
  makeAff \cb -> mempty <$ do
    cb <<< Left $ error $ "test timed out after " <> show time <> "ms"

timeout
  :: Int
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
  -> SpecM m Aff Unit Unit
  -> m (Producer Event Aff (Array (Tree Void Result)))
_run config specs = execWriterT specs <#> discardUnfocused >>> \tests -> do
  yield (Event.Start (countTests tests))
  let indexer index test = {test, path: [PathItem {name: Nothing, index}]}
  r <- loop $ mapWithIndex indexer tests
  yield (Event.End r)
  pure r
  where
    loop :: Array (TestWithPath ()) -> Producer Event Aff (Array (Tree Void Result))
    loop tests =
      let
        noteWithIsAllParallelizable = map \{test,path} -> { isParallelizable: isAllParallelizable test, test, path}
        groupByIsParallelizable = groupBy (\a b -> a.isParallelizable && b.isParallelizable)
      in join <$> for (groupByIsParallelizable $ noteWithIsAllParallelizable tests) \g ->
        join <$> if (NEA.head g).isParallelizable
          then mergeProducers (runGroup <$> (NEA.toArray g))
          else for (NEA.toArray g) runGroup

    runGroup :: TestWithPath (isParallelizable :: Boolean) -> Producer Event Aff (Array (Tree Void Result))
    runGroup {test, path, isParallelizable} = case test of
      (Leaf name (Just (Item item))) -> do
        yield $ Event.Test (if isParallelizable then Parallel else Sequential) path name
        let example = item.example \a -> a unit
        start <- lift $ liftEffect dateNow
        e <- lift $ attempt case config.timeout of
          Just t -> timeout t example
          _      -> example
        duration <- lift $ (_ - start) <$> liftEffect dateNow
        let res = either Failure (const $ Success (speedOf config.slow duration) duration) e
        yield $ Event.TestEnd path name res
        pure [ Leaf name $ Just res ]
      (Leaf name Nothing) -> do
        yield $ Event.Pending path name
        pure [ Leaf name Nothing ]
      (Node (Right cleanup) xs) -> do
        let indexer index x = {test:x, path: path <> [PathItem {name: Nothing, index}]}
        loop (mapWithIndex indexer xs) <* lift (cleanup unit)
      (Node (Left name) xs) -> do
        yield $ Event.Suite (if isParallelizable then Parallel else Sequential) path name
        let indexer index x = {test:x, path: path <> [PathItem {name: Just name, index}]}
        res <- loop (mapWithIndex indexer xs)
        yield $ Event.SuiteEnd path
        pure [ Node (Left name) res ]

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
        Left err -> lift $ joinFiber fib
        Right e -> do
          yield e
          loop
  loop


-- | Run a spec, returning the results, without any reporting
runSpec'
  :: forall m
   . Functor m
  => Config
  -> SpecM m Aff Unit Unit
  -> m (Aff (Array (Tree Void Result)))
runSpec' config spec = _run config spec <#> \runner -> P.runEffectRec $ runner //> const (pure unit)

-- | Run a spec with the default config, returning the results, without any
-- | reporting
runSpec
  :: Spec Unit
  -> Aff (Array (Tree Void Result))
runSpec = un Identity <<< runSpec' defaultConfig

type TestEvents = Producer Event Aff (Array (Tree Void Result))

type Reporter = Pipe Event Event Aff (Array (Tree Void Result))

-- | Run the spec with `config`, report results and (if configured as such)
-- | exit the program upon completion
run'
  :: forall m
   . Functor m
  => Config
  -> Array Reporter
  -> SpecM m Aff Unit Unit
  -> m (Aff Unit)
run' config reporters spec = _run config spec <#> \runner -> do
  let
    drain = const (pure unit)
    events = foldl (>->) runner reporters
    reportedEvents = P.runEffectRec $ events //> drain
  either onError onSuccess =<< try reportedEvents
  where
    onError :: Error -> Aff Unit
    onError err = liftEffect do
       logWriter $ tellLn $ styled Style.red (show err)
       when config.exit (exit 1)

    onSuccess :: Array (Tree Void Result) -> Aff Unit
    onSuccess results = liftEffect $
      when config.exit do
        let code = if successful results then 0 else 1
        exit code

-- | Run the spec with the default config
run
  :: Array Reporter
  -> Spec Unit
  -> Aff Unit
run reporters spec = un Identity $ run' defaultConfig reporters spec
