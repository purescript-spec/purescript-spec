module Test.Spec.Fixtures where

import Prelude

import Test.Spec            (Spec, describe, describeOnly, it, itOnly, pending)
import Test.Spec.Assertions (shouldEqual)

successTest :: Spec Unit
successTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1

sharedDescribeTest :: Spec Unit
sharedDescribeTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

duplicatedDescribeTest :: Spec Unit
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

describeOnlyTest :: Spec Unit
describeOnlyTest =
  describeOnly "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

describeOnlyNestedTest :: Spec Unit
describeOnlyNestedTest =
  describe "a" do
    describeOnly "b" do
      it "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

itOnlyTest :: Spec Unit
itOnlyTest =
  describe "a" do
    describe "b" do
      itOnly "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

failureTest :: Spec Unit
failureTest = it "fails" $ 1 `shouldEqual` 2

pendingTest :: Spec Unit
pendingTest = pending "is not written yet"
