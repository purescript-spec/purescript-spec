module Main where

import Prelude
import Control.Monad.Aff (later')
import Control.Monad.Eff (Eff)
import Test.Spec (SpecEffects, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Eff (SpecEffects ()) Unit
main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
      pending "feature complete"
    describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports streaming reporters" $ pure unit
      it "supports async specs" do
        res <- later' 100 $ pure "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.10.x compatible" $ pure unit
