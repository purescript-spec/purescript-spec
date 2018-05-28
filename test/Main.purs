module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.Spec.RunnerSpec (runnerSpec)

main :: Effect Unit
main = run [ consoleReporter ] do
  runnerSpec
  assertionSpec
