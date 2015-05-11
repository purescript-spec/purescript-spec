module Main where

import Data.Array
import Control.Monad.Eff.Exception
import Test.Spec
import Test.Spec.Node
import Test.Spec.Assertions
import qualified Test.Spec.Reporter as R

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

main = runNode $
  describe "Test" do
    describe "Spec" do
      describe "Runner" do
        it "collects \"it\" and \"pending\" in Describe groups" do
          results <- collect successTest
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success]]]
        it "collects \"it\" and \"pending\" with shared Describes" do
          results <- collect sharedDescribeTest
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success],
                                               Describe "c" [It "also works" Success]]]
      describe "Reporter" do
        it "collapses groups into entries with names" do
          results <- collect successTest
          concatMap R.collapse results `shouldEqual` [
              R.Describe ["a", "b"],
              R.It "works" Success
            ]
        it "collapses groups into entries with shared describes" do
          results <- collect sharedDescribeTest
          concatMap R.collapse results `shouldEqual` [
              R.Describe ["a", "b"],
              R.It "works" Success,
              R.Describe ["a", "c"],
              R.It "also works" Success
            ]
        it "reports failured tests" do
          results <- collect failureTest
          concatMap R.collapse results `shouldEqual` [
            R.It "fails" (Failure (error "1 ≠ 2"))
          ]
        it "reports pending tests" do
          results <- collect pendingTest
          concatMap R.collapse results `shouldEqual` [
            R.Pending "is not written yet"
          ]
