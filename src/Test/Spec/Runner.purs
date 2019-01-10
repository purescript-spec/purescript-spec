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
import Data.Array (all, groupBy, mapMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (class Traversable, for)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, forkAff, joinFiber, makeAff, throwError, try)
import Effect.Aff.AVar as AV
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Exception (Error, error)
import Effect.Exception as Error
import Pipes ((>->), yield)
import Pipes.Core (Pipe, Producer, (//>))
import Pipes.Core (runEffectRec) as P
import Test.Spec (Item(..), Result(..), Spec, SpecM, SpecTree, Tree(..))
import Test.Spec as Spec
import Test.Spec.Console (withAttrs)
import Test.Spec.Runner.Event (Event)
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed (speedOf)
import Test.Spec.Summary (successful)

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

filterFocusedIfAny :: forall c m a. Array (Tree c (Item m a)) -> Array (Tree c (Item m a))
filterFocusedIfAny ts = case mapMaybe findFocus ts of
  [] -> ts
  r -> r
  where
  findFocus :: Tree c (Item m a) -> Maybe (Tree c (Item m a))
  findFocus (Node n ts') = case mapMaybe findFocus ts' of
    [] -> Nothing
    r -> Just $ Node n r
  findFocus t@(Leaf n (Just (Item { isFocused }))) = if isFocused then Nothing else Just t
  findFocus (Leaf n Nothing) = Nothing

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

allParallelizable :: forall c m a. Tree c (Item m a) -> Boolean
allParallelizable = case _ of
  Node _ xs -> all allParallelizable xs
  Leaf _ x -> let p = x >>= un Item >>> _.isParallelizable in p == Just true || p == Nothing

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
_run config specs = execWriterT specs <#> filterFocusedIfAny >>> \tests -> do
  yield (Event.Start (Spec.countTests tests))
  r <- loop tests
  yield (Event.End r)
  pure r
  where
    loop :: Array (SpecTree Aff Unit) -> Producer Event Aff (Array (Tree Void Result))
    loop tests = 
      let
        marked :: Array (Tuple Boolean (SpecTree Aff Unit))
        marked = tests <#> \t -> Tuple (allParallelizable t) t
        grouped' :: Array (NonEmptyArray (Tuple Boolean (SpecTree Aff Unit)))
        grouped' = groupBy (\a b -> fst a && fst b) marked
        grouped :: Array (Tuple Boolean (Array (SpecTree Aff Unit)))
        grouped = grouped' <#> \g -> Tuple (fst $ NEA.head g) $ snd <$> NEA.toArray g
      in join <$> for grouped \(Tuple isParallelizable xs) -> join <$> if isParallelizable
        then mergeProducers (runGroup <$> xs)
        else for xs runGroup

    runGroup :: SpecTree Aff Unit -> Producer Event Aff (Array (Tree Void Result))
    runGroup (Leaf name (Just (Item item))) = do
      yield Event.Test
      let test = item.example \a -> a unit
      start <- lift $ liftEffect dateNow
      e <- lift $ attempt case config.timeout of
        Just t -> timeout t test
        _      -> test
      duration <- lift $ (_ - start) <$> liftEffect dateNow
      yield $ either
        (\err ->
          let msg = Error.message err
              stack = Error.stack err
          in Event.Fail name msg stack)
        (const $ Event.Pass name (speedOf config.slow duration) duration)
        e
      yield Event.TestEnd
      pure [ Leaf name $ Just $ either Failure (const Success) e ]

    runGroup (Leaf name Nothing) = do
      yield $ Event.Pending name
      pure [ Leaf name Nothing ]

    runGroup (Node (Right cleanup) xs) = do
      loop xs <* lift (cleanup unit)
    runGroup (Node (Left name) xs) = do
      yield $ Event.Suite name
      res <- loop xs
      yield Event.SuiteEnd
      pure [ Node (Left name) res ]
  
    -- Parallel -> mergeProducers (runGroup Parallel <$> xs)
    -- Sequential -> for xs (runGroup Sequential)
  -- runGroup :: Execution -> Group (Aff Unit) -> Producer Event Aff (Tree Void Result)
  -- runGroup isPar (It only name test) = do
  --   yield Event.Test
  --   start    <- lift $ liftEffect dateNow
  --   e        <- lift $ attempt case config.timeout of
  --                                     Just t -> timeout t test
  --                                     _      -> test
  --   duration <- lift $ (_ - start) <$> liftEffect dateNow
  --   yield $ either
  --     (\err ->
  --       let msg = Error.message err
  --           stack = Error.stack err
  --        in Event.Fail name msg stack)
  --     (const $ Event.Pass name (speedOf config.slow duration) duration)
  --     e
  --   yield Event.TestEnd
  --   pure $ It only name $ either Failure (const Success) e

  -- runGroup isPar (Pending name) = do
  --   yield $ Event.Pending name
  --   pure $ Pending name

  -- runGroup _ (SetExecution isPar xs) = do
  --   SetExecution isPar <$> loop xs isPar

  -- runGroup isPar (Describe only name xs) = do
  --   yield $ Event.Suite name
  --   Describe only name <$> loop xs isPar
  --     <* yield Event.SuiteEnd
  
  -- loop xs = case _ of
  --   Parallel -> mergeProducers (runGroup Parallel <$> xs)
  --   Sequential -> for xs (runGroup Sequential)


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
       withAttrs [31] $ logShow err
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
