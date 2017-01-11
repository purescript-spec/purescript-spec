module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec (SpecEffects)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.Reporter (specReporter)
import Test.Spec.ReporterSpec (reporterSpec)
import Test.Spec.Runner (run)
import Test.Spec.RunnerSpec (runnerSpec)

main :: forall eff. Eff (SpecEffects eff) Unit
main = run [specReporter] do
  runnerSpec
  reporterSpec
  assertionSpec
