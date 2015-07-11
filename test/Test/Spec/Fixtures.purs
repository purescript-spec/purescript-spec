module Test.Spec.Fixtures where

import Prelude

import Test.Spec            (describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

successTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1

sharedDescribeTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

failureTest = it "fails" $ 1 `shouldEqual` 2

pendingTest = pending "is not written yet"

