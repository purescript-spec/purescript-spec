module Test.Spec.Fixtures where

import Test.Spec
import Test.Spec.Assertions

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

