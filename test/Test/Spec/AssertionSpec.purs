module Test.Spec.AssertionSpec where

import Prelude
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldNotContain, shouldNotSatisfy, shouldSatisfy) as A
import Test.Spec.Assertions.Aff (expectError, shouldReturn, shouldNotReturn) as AF
import Test.Spec.Assertions.String (shouldContain, shouldNotContain, shouldStartWith, shouldEndWith) as AS

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

          describe "shouldStartWith" do
            it "accepts strings that start with prefix" $
              "hello, world" `AS.shouldStartWith` "hello"
            it "rejects strings that do not start with prefix" $
              AF.expectError $ "hello" `AS.shouldStartWith` "hello, world"

          describe "shouldEndWith" do
            it "accepts strings that end with suffix" $
              "hello, world" `AS.shouldEndWith` "world"
            it "rejects strings that do not end with suffix" $
              AF.expectError $ "world" `AS.shouldEndWith` "hello, world"

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


        describe "Aff" do
          let contained = "nono"
              notcontained = "zzz"
              f = pure contained

          describe "expectError" do
            it "returns unit when given an error" $
              AF.expectError $ throwError $ error "omg"
            it "returns an error when given a non-error" $
              AF.expectError $ AF.expectError $ pure "ok"

          describe "shouldReturn" do
            it "accepts that `Aff String` contains \"nono\"" $
              f `AF.shouldReturn` contained
            it "rejects that `Aff String` contains \"zzz\"" $
              AF.expectError $ f `AF.shouldReturn` notcontained

          describe "shouldNotReturn" do
            it "accepts f does not contain \"zzz\"" $
              f `AF.shouldNotReturn` notcontained
            it "rejects that `Aff String` does not contain \"zzz\"" $
              AF.expectError $ f `AF.shouldNotReturn` contained
