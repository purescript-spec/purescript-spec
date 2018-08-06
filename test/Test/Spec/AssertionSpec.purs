module Test.Spec.AssertionSpec where

import Prelude
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldNotContain, shouldNotSatisfy, shouldSatisfy) as A
import Test.Spec.Assertions.Aff (expectError) as AF
import Test.Spec.Assertions.String (shouldContain, shouldNotContain) as AS

assertionSpec :: Spec Unit
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

        describe "Predicates" do
          describe "shouldSatisfy" do
            it "accepts values where predicate returns true" $
              3 `A.shouldSatisfy` (_ > 2)
            it "rejects values where predicate returns false" $
              AF.expectError $ 3 `A.shouldSatisfy` (_ < 2)

          describe "shouldNotSatisfy" do
            it "accepts values where predicate returns false" $
              3 `A.shouldNotSatisfy` (_ < 2)
            it "rejects values where predicate returns true" $
              AF.expectError $ 3 `A.shouldNotSatisfy` (_ > 2)

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
