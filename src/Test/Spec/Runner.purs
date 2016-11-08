module Test.Spec.Runner where

import Prelude

import Pipes as P
import Pipes (yield)
import Pipes.Core (Producer(), (//>))
import Pipes.Core as P
import Pipes.Prelude as P

import Data.Array       (singleton)
import Data.Traversable (for, sequence_)
import Data.Foldable    (foldl)
import Data.Tuple       (snd)
import Data.Either      (either)
import Data.Maybe       (Maybe(..), fromMaybe)

import Control.Monad.Aff           (Aff(), runAff, attempt)
import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Console   (CONSOLE(), logShow)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception as Error
import Control.Monad.Trans.Class   (lift)
import Control.Alternative ((<|>))

import Node.Process (PROCESS())
import Node.Process as Process

import Test.Spec.Runner.Event (Event)
import Test.Spec.Runner.Event as Event
import Test.Spec              (Spec(), Group(..), Result(..), collect)
import Test.Spec.Reporter     (BaseReporter())
import Test.Spec.Reporter     as Reporter
import Test.Spec.Console      (withAttrs)
import Test.Spec.Summary      (successful)

type RunEffects e = (process :: PROCESS, console :: CONSOLE | e)

trim :: ∀ r. Array (Group r) -> Array (Group r)
trim xs = fromMaybe xs (singleton <$> findJust findOnly xs)
  where
  findOnly :: forall r. Group r -> Maybe (Group r)
  findOnly g@(It true _ _) = pure g
  findOnly g@(Describe o _ gs) = findJust findOnly gs <|> if o then pure g else Nothing
  findOnly _ = Nothing

  findJust :: forall a. (a -> Maybe a) -> Array a -> Maybe a
  findJust f = foldl go Nothing
    where
    go Nothing x = f x
    go acc _ = acc

run'
  :: ∀ e
   . Spec e Unit
  -> Producer Event (Aff e) (Array (Group Result))
run' spec = do
  yield Event.Start
  for (trim $ collect spec) runGroup
  <* yield Event.End

  where
  runGroup (It only name test) = do
    yield Event.Test
    e <- lift $ attempt test
    yield $ either
      (Event.Fail name <<< Error.message)
      (const $ Event.Pass name)
      e
    pure $ It only name $ either Failure (const Success) e

  runGroup (Pending name) = do
    yield $ Event.Pending name
    pure $ Pending name

  runGroup (Describe only name xs) = do
    yield $ Event.Suite name
    Describe only name <$> (for xs runGroup)
    <* yield Event.SuiteEnd

runSpec
  :: ∀ e
   . Spec e Unit
  -> Aff e (Array (Group Result))
runSpec spec = P.runEffect $ run' spec //> const (pure unit)

run
  :: ∀ s e
   . Array (BaseReporter s (Eff (RunEffects e)))
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
