module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.Spec.RunnerSpec (runnerSpec)

main :: Effect Unit
main = launchAff_ $ run [ consoleReporter ] do
  runnerSpec
  assertionSpec
