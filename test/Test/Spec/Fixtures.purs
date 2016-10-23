module Test.Spec.Fixtures where

import Prelude

import Test.Spec            (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

successTest :: forall r. Spec r Unit
successTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1

sharedDescribeTest :: forall r. Spec r Unit
sharedDescribeTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

failureTest :: forall r. Spec r Unit
failureTest = it "fails" $ 1 `shouldEqual` 2

pendingTest :: forall r. Spec r Unit
pendingTest = pending "is not written yet"
