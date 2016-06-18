module Test.Spec.Runner (
  run
  ) where

import Prelude

import Control.Monad.Aff         (runAff)
import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE(), logShow)
import Data.Foldable             (sequence_)

import Test.Spec          (Spec(), collect)
import Test.Spec.Console  (withAttrs)
import Test.Spec.Summary  (successful)
import Test.Spec.Reporter (Reporter())

import Node.Process (PROCESS())
import Node.Process as Process

-- Runs the tests and invoke all reporters.
-- If run in a NodeJS environment any failed test will cause the
-- process to exit with a non-zero exit code. On success it will
-- exit with a zero exit code explicitly, so passing integration tests that still have
-- connections open can run in CI successfully.
run :: forall e.
    Array (Reporter (process :: PROCESS, console :: CONSOLE | e))
    -> Spec (process :: PROCESS, console :: CONSOLE | e) Unit
    -> Eff  (process :: PROCESS, console :: CONSOLE | e) Unit
run rs spec = do
  _ <- runAff
    (\err -> do withAttrs [31] $ logShow err
                Process.exit 1)
    (\results -> do sequence_ (map (\f -> f results) rs)
                    if (successful results)
                      then Process.exit 0
                      else Process.exit 1)
    (collect spec)
  pure unit
