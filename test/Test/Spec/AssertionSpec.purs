module Test.Spec.AssertionSpec where

import Prelude

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class   (throwError)

import Test.Spec                    (Spec, describe, it)
import Test.Spec.Assertions.Aff     (expectError)
import Test.Spec.Assertions.String  (shouldContain, shouldNotContain)

assertionSpec :: ∀ e. Spec e Unit
assertionSpec =
  describe "Test" $
    describe "Spec" $
      describe "Assertions" do

        describe "String" do
          describe "shouldContain" do
            it "accepts strings that contains substrings" $
              "foobar" `shouldContain` "foo"
            it "rejects strings that does not contain substrings" $
              expectError $ "baz" `shouldContain` "foo"

          describe "shouldNotContain" do
            it "accepts strings that does not contain substrings" $
              "foobar" `shouldNotContain` "baz"
            it "rejects strings that contains substrings" $
              expectError $ "bazbar" `shouldNotContain` "baz"

        describe "Aff" $
          describe "expectError" do
            it "returns unit when given an error" $
              expectError $ throwError $ error "omg"
            it "returns an error when given a non-error" $
              expectError $ expectError $ pure "ok"
