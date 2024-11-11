module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Test.Spec (Spec, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] spec

spec :: Spec Unit
spec =
  describe "async delayed tests" $ parallel do
    it "test1" do
      delay (Milliseconds 100.0)
      true `shouldEqual` true
    it "test2" do
      delay (Milliseconds 100.0)
      true `shouldEqual` true
    it "test3" do
      delay (Milliseconds 100.0)
      true `shouldEqual` true
    it "test4" do
      delay (Milliseconds 100.0)
      true `shouldEqual` false
    it "test5" do
      delay (Milliseconds 100.0)
      true `shouldEqual` true
