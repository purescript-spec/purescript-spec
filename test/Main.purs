module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.HoistSpec (hoistSpecSpec)
import Test.Spec.HookSpec (hookSpec)
import Test.Spec.ParallelSpec (parallelSpec)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec.RunnerSpec (runnerSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter] do
  runnerSpec
  assertionSpec
  hookSpec
  hoistSpecSpec
  parallelSpec
