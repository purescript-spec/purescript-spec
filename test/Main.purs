module Test.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.HoistSpec (hoistSpecSpec)
import Test.Spec.HookSpec (hookSpec)
import Test.Spec.ParallelSpec (parallelSpec)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (defaultConfig, run')
import Test.Spec.RunnerSpec (runnerSpec)

main :: Effect Unit
main = launchAff_ $ un Identity $ run' (defaultConfig{timeout = Nothing}) [ specReporter ] do
  runnerSpec
  assertionSpec
  hookSpec
  hoistSpecSpec
  parallelSpec

