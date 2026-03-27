module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Test.Spec (Spec, describe, it, parallel, sequential)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] spec

spec :: Spec Unit
spec = describe "mixed tests" do
  describe "sync tests - first batch" do
    it "test 1" do
      true `shouldEqual` true
    it "test 2" do
      true `shouldEqual` false
    it "test 3" do
      true `shouldEqual` true

  describe "async delayed tests" $ parallel do
    it "test 4" do
      delay (Milliseconds 100.0)
      true `shouldEqual` false
    it "test 5" do
      delay (Milliseconds 70.0)
      true `shouldEqual` true
    it "test 6" do
      delay (Milliseconds 60.0)
      true `shouldEqual` true

    describe "nested sync tests" $ sequential do
      it "test 7" do
        delay (Milliseconds 100.0)
        true `shouldEqual` false
      it "test 8" do
        delay (Milliseconds 100.0)
        true `shouldEqual` true

    it "test 9" do
      delay (Milliseconds 50.0)
      true `shouldEqual` false
    it "test 10" do
      delay (Milliseconds 100.0)
      true `shouldEqual` true

  describe "sync tests - second batch" do
    it "test 11" do
      true `shouldEqual` true
    it "test 12" do
      true `shouldEqual` false
    it "test 13" do
      true `shouldEqual` true
