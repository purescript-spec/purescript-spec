module Test.Spec.AssertionSpec where

import Prelude

import Test.Spec
import Test.Spec.Assertions.String

assertionSpec =
  describe "Test" $
    describe "Spec" $
      describe "Assertions" $
        describe "String" do
          it "checks if a string contains another string" do
            "foobar" `shouldContain` "foo"
