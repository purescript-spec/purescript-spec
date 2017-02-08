module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Test.Spec.RunnerSpec (runnerSpec)

main :: Eff (RunnerEffects ()) Unit
main = run [ consoleReporter ] do
  runnerSpec
  assertionSpec
