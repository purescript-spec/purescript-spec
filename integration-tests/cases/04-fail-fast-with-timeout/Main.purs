module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Config (defaultConfig)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec')

main :: Effect Unit
main = launchAff_ $ runSpec' config [specReporter] do
  it "passes quickly" $
    5 `shouldEqual` (3 + 2)

  it "times out" $
    delay (Milliseconds 15.0)

  it "shouldn't get to run" $
    2 `shouldEqual` 3

  where
    config = defaultConfig { failFast = true, timeout = Just $ Milliseconds 10.0 }
