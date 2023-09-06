module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Config (defaultConfig)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec')

main :: Effect Unit
main = launchAff_ $ runSpec' defaultConfig [consoleReporter] do
  describe "A" do
    it "should be under A" (pure unit)
  describe "B" do
    it "should be under B" (pure unit)
  describe "C" do
    it "should be under C" (pure unit)
