module Test.Spec.Fixtures where

import Prelude

import Test.Spec            (Spec, describe, describeOnly, it, itOnly, pending)
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

duplicatedDescribeTest :: forall r. Spec r Unit
duplicatedDescribeTest =
  describe "a" do
    describe "b" do
      describe "c" do
        it "first" do
          1 `shouldEqual` 1
    describe "b" do
      describe "c" do
        it "second" do
          1 `shouldEqual` 1

describeOnlyTest :: forall r. Spec r Unit
describeOnlyTest =
  describeOnly "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

itOnlyTest :: forall r. Spec r Unit
itOnlyTest =
  describe "a" do
    describe "b" do
      itOnly "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

failureTest :: forall r. Spec r Unit
failureTest = it "fails" $ 1 `shouldEqual` 2

pendingTest :: forall r. Spec r Unit
pendingTest = pending "is not written yet"
