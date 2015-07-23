module Main where

import Prelude

import Control.Monad.Aff
import Test.Spec (describe, pending, it)
import Test.Spec.Node
import Test.Spec.Assertions
import Test.Spec.Reporter.Console

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
      it "PureScript 0.7 compatible" $ return unit
    describe "TODO" do
      pending "browser support!"
