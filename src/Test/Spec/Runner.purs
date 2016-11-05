module Test.Spec.Runner
  ( run
  , runSpec
  ) where

import Prelude

import Control.Monad.Aff           (Aff(), runAff, attempt)
import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Console   (CONSOLE(), logShow)
import Control.Monad.Eff.Exception (Error)
import Control.Alt                 ((<|>))

import Data.Array                (singleton)
import Data.Maybe                (Maybe(..), fromMaybe)
import Data.Either               (either)
import Data.Foldable             (foldl, sequence_)
import Data.Traversable          (sequence)

import Test.Spec                (Spec(), Group(..), Result(..), collect)
import Test.Spec.Console        (withAttrs)
import Test.Spec.Summary        (successful)
import Test.Spec.Reporter       (Reporter())

import Node.Process (PROCESS())
import Node.Process as Process

runCatch :: forall r. Group (Aff r Unit)
         -> Aff r (Group Result)
runCatch group =
  case group of
    It only name test -> do
      let onError e = pure $ It only name $ Failure e
          onSuccess _ = pure $ It only name Success
      e <- attempt test
      either onError onSuccess e
    Describe only name groups -> do
      results <- sequence (map runCatch groups)
      pure (Describe only name results)
    Pending name -> pure (Pending name)

trim :: ∀ r. Array (Group r) -> Array (Group r)
trim xs = fromMaybe xs (singleton <$> findJust findOnly xs)
  where
  findOnly :: Group r -> Maybe (Group r)
  findOnly g@(It true _ _) = pure g
  findOnly g@(Describe o _ gs) = findJust findOnly gs <|> if o then pure g
                                                               else Nothing
  findOnly _ = Nothing

  findJust :: forall a. (a -> Maybe a) -> Array a -> Maybe a
  findJust f = foldl go Nothing
    where
    go Nothing x = f x
    go acc _     = acc

runSpec :: forall r. Spec r Unit
         -> Aff r (Array (Group Result))
runSpec = sequence <<< map runCatch <<< trim <<< collect

-- Runs the tests and invoke all reporters.
-- If run in a NodeJS environment any failed test will cause the
-- process to exit with a non-zero exit code. On success it will
-- exit with a zero exit code explicitly, so passing integration tests that still have
-- connections open can run in CI successfully.
run :: forall e.
    Array (Reporter (process :: PROCESS, console :: CONSOLE | e))
    -> Spec (process :: PROCESS, console :: CONSOLE | e) Unit
    -> Eff  (process :: PROCESS, console :: CONSOLE | e) Unit
run rs spec = void $ runAff onError onSuccess (runSpec spec)
  where
    onError :: Error -> Eff (process :: PROCESS, console :: CONSOLE | e) Unit
    onError err = do withAttrs [31] $ logShow err
                     Process.exit 1
    onSuccess :: Array (Group Result) -> Eff (process :: PROCESS, console :: CONSOLE | e) Unit
    onSuccess results = do sequence_ (map (\f -> f results) rs)
                           if (successful results)
                             then Process.exit 0
                             else Process.exit 1
