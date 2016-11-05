module Test.Spec.PipeRunner where

import Prelude

import Pipes as P
import Pipes (yield, await, (>~), (>->))
import Pipes.Core as P
import Pipes.Core (Producer())
import Pipes.Prelude as P

import Data.Traversable (for, sequence_)
import Data.Tuple       (fst, snd)
import Data.Either      (either)

import Control.Monad.Aff           (Aff(), runAff, attempt, launchAff, liftEff')
import Control.Monad.Aff.Console   (log)
import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Console   (CONSOLE(), logShow)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception as Error
import Control.Monad.Trans.Class   (lift)

import Node.Process (PROCESS())
import Node.Process as Process
import Test.Spec.Runner.Event (Event)
import Test.Spec.Runner.Event as Event

import Test.Spec                (Spec(), Group(..), Result(..), collect)
import Test.Spec.Reporter       (Reporter(), BaseReporter())
import Test.Spec.Reporter       as Reporter
import Test.Spec.Console        (withAttrs)
import Test.Spec.Summary        (successful)

type RunEffects e = (process :: PROCESS, console :: CONSOLE | e)

run' :: forall e.
  Spec (RunEffects e) Unit
  -> Producer Event
              (Aff (RunEffects e))
              (Array (Group Result))
run' spec = do
  yield Event.Start
  for (collect spec) runGroup
  <* yield Event.End

  where
  runGroup (It name test) = do
    yield Event.Test
    e <- lift $ attempt test
    yield $ either
      (Event.Fail name <<< Error.message)
      (const $ Event.Pass name)
      e
    pure $ It name $ either Failure (const Success) e

  runGroup (Pending name) = do
    yield $ Event.Pending name
    pure $ Pending name

  runGroup (Describe name xs) = do
    yield $ Event.Suite name
    Describe name <$> (for xs runGroup)
    <* yield Event.SuiteEnd

run :: forall s e.
    Array (BaseReporter s (Eff (RunEffects e)))
    -> Spec (RunEffects e) Unit
    -> Eff  (RunEffects e) Unit
run reporters spec = void do
  runAff onError onSuccess do
    snd <$> do
      P.foldM' step begin done do
        run' spec

  where
    begin       = pure reporters
    step rs evt = for rs (liftEff <<< Reporter.update evt)
    done _      = pure unit

    onError :: Error -> Eff (RunEffects e) Unit
    onError err = do withAttrs [31] $ logShow err
                     Process.exit 1

    onSuccess :: Array (Group Result) -> Eff (RunEffects e) Unit
    onSuccess results = do sequence_ (map (Reporter.summarize results) reporters)
                           if (successful results)
                             then Process.exit 0
                             else Process.exit 1
