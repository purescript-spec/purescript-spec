module Test.Spec.Runner (
  Process(..),
  run
  ) where

import Prelude

import Control.Monad.Aff         (runAff)
import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE(), print)
import Data.Foldable             (sequence_)

import Test.Spec          (Spec(), collect)
import Test.Spec.Console  (withAttrs)
import Test.Spec.Summary  (successful)
import Test.Spec.Reporter (Reporter())

foreign import data Process :: !

foreign import exit :: forall eff. Int -> Eff (process :: Process | eff) Unit

-- Runs the tests and invoke all reporters.
-- If run in a NodeJS environment any failed test will cause the
-- process to exit with a non-zero exit code. On success it will
-- exit with a zero exit code explicitly, so passing integration tests that still have
-- connections open can run in CI successfully.
run :: forall e.
    Array (Reporter (process :: Process, console :: CONSOLE | e))
    -> Spec (process :: Process, console :: CONSOLE | e) Unit
    -> Eff  (process :: Process, console :: CONSOLE | e) Unit
run rs spec = do
  runAff
    (\err -> do withAttrs [31] $ print err
                exit 1)
    (\results -> do sequence_ (map (\f -> f results) rs)
                    if (successful results)
                      then exit 0
                      else exit 1)
    (collect spec)

