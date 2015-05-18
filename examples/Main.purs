module Main where

import Control.Monad.Aff
import Test.Spec (describe, pending, it)
import Test.Spec.Node
import Test.Spec.Assertions
import Test.Spec.Reporter.Console
import Test.QuickCheck

main = runNode [consoleReporter] do
  describe "purescript-spec" do
    describe "What is it?" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
    describe "Features" do
      it "run specs in NodeJS" $ return unit
      it "supports async specs" do
        res <- later' 100 $ return "Alligator"
        res `shouldEqual` "Alligator"
      it "can output Xunit reports" $ return unit
    describe "TODO" do
      pending "browser support!"
