module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Integration (integrationSpecs)
import Test.Spec (hoistSpec)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.HoistSpec (hoistSpecSpec)
import Test.Spec.HookSpec (hookSpec)
import Test.Spec.ParallelSpec (parallelSpec)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Reporter.TeamCitySpec (teamcitySpec)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.Spec.RunnerSpec (runnerSpec)

main :: Effect Unit
main = launchAff_ $ void $ join $
  runSpecT defaultConfig { timeout = Just $ Milliseconds 60000.0 } [specReporter] do
    pureSpecs
    integrationSpecs
  where
    pureSpecs = hoistSpec (pure <<< unwrap) (\_ x -> x) do
      runnerSpec
      assertionSpec
      hookSpec
      hoistSpecSpec
      parallelSpec
      teamcitySpec
