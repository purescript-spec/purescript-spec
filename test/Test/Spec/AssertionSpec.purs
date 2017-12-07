module Test.Spec.AssertionSpec where

import Prelude
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Spec (Spec, describe, it)
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

        describe "Aff" $
          describe "expectError" do
            it "returns unit when given an error" $
              AF.expectError $ throwError $ error "omg"
            it "returns an error when given a non-error" $
              AF.expectError $ AF.expectError $ pure "ok"
