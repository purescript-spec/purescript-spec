module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.HoistSpec (hoistSpecSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, run')
import Test.Spec.RunnerSpec (runnerSpec)

main :: Effect Unit
main = launchAff_ $ run' (defaultConfig{timeout = Nothing}) [ consoleReporter ] do
  runnerSpec
  assertionSpec
  hoistSpecSpec
