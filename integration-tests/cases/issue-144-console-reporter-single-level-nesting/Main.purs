module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "A" do
    it "should be under A" (pure unit)
  describe "B" do
    it "should be under B" (pure unit)
  describe "C" do
    it "should be under C" (pure unit)
