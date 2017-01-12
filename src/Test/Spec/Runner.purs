module Test.Spec.Runner
       ( RunnerEffects
       , run
       , run'
       , runSpec
       , runSpec'
       , defaultConfig
       , timeout
       , Config
       ) where

import Prelude
import Control.Monad.Eff.Exception as Error
import Node.Process as Process
import Test.Spec as Spec
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner.Event as Event
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, makeAff, runAff, forkAff, cancelWith, attempt)
import Control.Monad.Aff.AVar (makeVar, killVar, putVar, takeVar, AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Trans.Class (lift)
import Data.Array (singleton)
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, sequence_)
import Data.Tuple (snd)
import Node.Process (PROCESS)
import Pipes (yield)
import Pipes.Core (Producer, (//>))
import Pipes.Core (runEffect) as P
import Pipes.Prelude (foldM') as P
import Test.Spec (Spec, Group(..), Result(..), SpecEffects, collect)
import Test.Spec.Console (withAttrs)
import Test.Spec.Reporter (BaseReporter)
import Test.Spec.Runner.Event (Event)
import Test.Spec.Speed (speedOf)
import Test.Spec.Summary (successful)

type RunnerEffects e = SpecEffects (process :: PROCESS | e)

foreign import dateNow :: ∀ e. Eff e Int

type Config = {
  slow :: Int
, timeout :: Maybe Int
}

defaultConfig :: Config
defaultConfig = {
  slow: 75
, timeout: Just 2000
}

trim :: ∀ r. Array (Group r) -> Array (Group r)
trim xs = fromMaybe xs (singleton <$> findJust findOnly xs)
  where
  findOnly :: Group r -> Maybe (Group r)
  findOnly g@(It true _ _) = pure g
  findOnly g@(Describe o _ gs) = findJust findOnly gs <|> if o then pure g else Nothing
  findOnly _ = Nothing

  findJust :: forall a. (a -> Maybe a) -> Array a -> Maybe a
  findJust f = foldl go Nothing
    where
    go Nothing x = f x
    go acc _ = acc

pickFirst
  :: ∀ e
   . Aff (avar :: AVAR | e) Unit
  -> Aff (avar :: AVAR | e) Unit
  -> Aff (avar :: AVAR | e) Unit
pickFirst t1 t2 = do
  va <- makeVar
  c1 <- forkAff $ attempt t1 >>= either (killVar va) (putVar va)
  c2 <- forkAff $ attempt t2 >>= either (killVar va) (putVar va)
  (takeVar va) `cancelWith` (c1 <> c2)

makeTimeout
  :: ∀ e
   . Int
  -> Aff (timer :: TIMER | e) Unit
makeTimeout time = makeAff \fail _ -> void do
  setTimeout time $ fail $ error $ "test timed out after " <> show time <> "ms"

timeout
  :: ∀ e
   . Int
  -> Aff (RunnerEffects e) Unit
  -> Aff (RunnerEffects e) Unit
timeout time t = t `pickFirst` (makeTimeout time)

-- Run the given spec as `Producer` in the underlying `Aff` monad.
-- This producer has two responsibilities:
--      1) emit events for key moments in the runner's lifecycle
--      2) collect the tst output into an array of results
-- This allows downstream consumers to report about the tests even before the
-- prodocer has completed and still benefit from the array of results the way
-- the runner sees it.
_run
  :: ∀ e
   . Config
  -> Spec (RunnerEffects e) Unit
  -> Producer Event (Aff (RunnerEffects e)) (Array (Group Result))
_run config spec = do
  yield (Event.Start (Spec.countTests spec))
  for (trim $ collect spec) runGroup
  <* yield Event.End

  where
  runGroup (It only name test) = do
    yield Event.Test
    start    <- lift $ liftEff dateNow
    e        <- lift $ attempt case config.timeout of
                                      Just t -> timeout t test
                                      _      -> test
    duration <- lift $ (_ - start) <$> liftEff dateNow
    yield $ either
      (\err ->
        let msg = Error.message err
            stack = Error.stack err
         in Event.Fail name msg stack)
      (const $ Event.Pass name (speedOf config.slow duration) duration)
      e
    yield Event.TestEnd
    pure $ It only name $ either Failure (const Success) e

  runGroup (Pending name) = do
    yield $ Event.Pending name
    pure $ Pending name

  runGroup (Describe only name xs) = do
    yield $ Event.Suite name
    Describe only name <$> (for xs runGroup)
    <* yield Event.SuiteEnd

-- Run a spec, returning the results, without any reporting
runSpec'
  :: ∀ e
   . Config
  -> Spec (RunnerEffects e) Unit
  -> Aff (RunnerEffects e) (Array (Group Result))
runSpec' config spec = P.runEffect $ _run config spec //> const (pure unit)

runSpec
  :: ∀ e
   . Spec (RunnerEffects e) Unit
  -> Aff (RunnerEffects e) (Array (Group Result))
runSpec spec = P.runEffect $ _run defaultConfig spec //> const (pure unit)

-- Run the spec, report results and exit the program upon completion
run'
  :: ∀ c s e
   . Config
  -> Array (BaseReporter c s (Eff (RunnerEffects e)))
  -> Spec (RunnerEffects e) Unit
  -> Eff  (RunnerEffects e) Unit
run' config reporters spec = void do
  runAff onError onSuccess do
    snd <$> do
      P.foldM' step begin done do
        _run config spec

  where
    begin       = pure reporters
    step rs evt = for rs (liftEff <<< Reporter.update evt)
    done _      = pure unit

    onError :: Error -> Eff (RunnerEffects e) Unit
    onError err = do withAttrs [31] $ logShow err
                     Process.exit 1

    onSuccess :: Array (Group Result) -> Eff (RunnerEffects e) Unit
    onSuccess results = do sequence_ (map (Reporter.summarize results) reporters)
                           if successful results
                             then Process.exit 0
                             else Process.exit 1

run
  :: ∀ c s e
   . Array (BaseReporter c s (Eff (RunnerEffects e)))
  -> Spec (RunnerEffects e) Unit
  -> Eff  (RunnerEffects e) Unit
run = run' defaultConfig
