module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [specReporter] do
  it "one" $
    5 `shouldEqual` (3 + 2)

  describe "two" do
    it "first" $ pure unit
    it "second" $ pure unit
    it "third" $ delay (Milliseconds 20.0)

  describe "three" do
    describe "1" do
      it "uno" $ pure unit
      it "dos" $ pure unit
    describe "2" do
      it "ein" $ pure unit
