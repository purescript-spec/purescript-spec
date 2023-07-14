module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [specReporter] do
  it "one" $
    5 `shouldEqual` (3 + 3)

  describe "two" do
    it "first" $ fail "boom"
    it "second" $ fail "crash"

  describe "three" do
    describe "1" do
      it "uno" $ fail "ohmigawd"
      it "dos" $ fail "aaargh!"
    describe "2" do
      it "ein" $ fail "die"
