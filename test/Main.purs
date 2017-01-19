module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.Reporter.Dot (dotReporter)
import Test.Spec.ReporterSpec (reporterSpec)
import Test.Spec.Runner (RunnerEffects, run)
import Test.Spec.RunnerSpec (runnerSpec)

main :: Eff (RunnerEffects ()) Unit
main = run (dotReporter { width: 80, slow: 800 }) do
  runnerSpec
  reporterSpec
  assertionSpec
