module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Config (defaultConfig)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpecPure')

main :: Effect Unit
main = launchAff_ $ runSpecPure' config [specReporter] do
  it "passes" $
    5 `shouldEqual` (3 + 2)

  it "fails" $
    fail "This is a failing test"

  it "shouldn't get to run" $
    2 `shouldEqual` 3

  where
    config = defaultConfig { failFast = true, exit = false }
