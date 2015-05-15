module Main where

import Control.Monad.Aff
import Test.Spec (describe, pending, it)
import Test.Spec.Node
import Test.Spec.Assertions
import Test.Spec.Reporter.Console
import Test.QuickCheck

additionSpec =
  describe "Addition" do
    it "does addition" do
      (1 + 1) `shouldEqual` 2
    it "fails as well" do
      (1 + 1) `shouldEqual` 3

main = runNode [consoleReporter] do
  describe "Math" do
    additionSpec
    describe "Multiplication" do
      pending "will do multiplication in the future"
  describe "Async" do
    it "asserts in the future" do
      res <- later' 100 $ return "Alligator"
      res `shouldEqual` "Alligator"
