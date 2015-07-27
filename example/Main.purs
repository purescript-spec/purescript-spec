module Main where

import Prelude

import Control.Monad.Aff
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "What is it?" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
    describe "Features" do
      it "runs in NodeJS" $ return unit
      it "runs in the browser" $ return unit
      it "supports async specs" do
        res <- later' 100 $ return "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.7 compatible" $ return unit
