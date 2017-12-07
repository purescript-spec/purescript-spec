module Test.Spec.AssertionSpec where

import Prelude
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldNotContain) as A
import Test.Spec.Assertions.Aff (expectError) as AF
import Test.Spec.Assertions.String (shouldContain, shouldNotContain) as AS
import Test.Spec.Runner (RunnerEffects)

assertionSpec :: ∀ e. Spec (RunnerEffects e) Unit
assertionSpec =
  describe "Test" $
    describe "Spec" $
      describe "Assertions" do

        describe "String" do
          describe "shouldContain" do
            it "accepts strings that contains substrings" $
              "foobar" `AS.shouldContain` "foo"
            it "rejects strings that does not contain substrings" $
              AF.expectError $ "baz" `AS.shouldContain` "foo"

          describe "shouldNotContain" do
            it "accepts strings that does not contain substrings" $
              "foobar" `AS.shouldNotContain` "baz"
            it "rejects strings that contains substrings" $
              AF.expectError $ "bazbar" `AS.shouldNotContain` "baz"

        describe "Foldable" do
          describe "for some foldable" do
            let f = ["haha", "nono"]
            let contained = "nono"
            let notcontained = "zzz"

            describe "shouldContain" do
              it "accepts f that contains a" $
                f `A.shouldContain` contained
              it "rejects f that does not contain a" $
                AF.expectError $ f `A.shouldContain` notcontained

            describe "shouldNotContain" do
              it "accepts f that does not contain a" $
                f `A.shouldNotContain` notcontained
              it "rejects f that contains a" $
                AF.expectError $ f `A.shouldNotContain` contained


        describe "Aff" $
          describe "expectError" do
            it "returns unit when given an error" $
              AF.expectError $ throwError $ error "omg"
            it "returns an error when given a non-error" $
              AF.expectError $ AF.expectError $ pure "ok"
