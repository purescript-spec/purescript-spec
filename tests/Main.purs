module Main where

import Data.Array
import Test.Spec
import Test.Spec.Runner
import Test.Spec.Assertions
import qualified Test.Spec.Reporter as R

successTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1

successSharedDescribeTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

main = suite $
  describe "Test" do
    describe "Spec" do
      describe "Runner" do
        it "collects \"it\" and \"pending\" in Describe groups" do
          results <- collect successTest
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success]]]
        it "collects \"it\" and \"pending\" with shared Describes" do
          results <- collect successSharedDescribeTest
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
          results <- collect successSharedDescribeTest
          concatMap R.collapse results `shouldEqual` [
              R.Describe ["a", "b"],
              R.It "works" Success,
              R.Describe ["a", "c"],
              R.It "also works" Success
            ]
        it "collapses groups with Its" do
          (R.collapse
            (Describe "a"
              [Describe "b" [Describe "c" [It "does c" Success]],
               Describe "d" [Describe "e" [It "does e" Success]]]))
            `shouldEqual`
            [R.Describe ["a", "b", "c"], R.It "does c" Success,
            R.Describe ["a", "d", "e"], R.It "does e" Success]
