module Test.Spec.ReporterSpec where

import Data.Array (concatMap)
import Control.Monad.Eff.Exception
import Test.Spec
import Test.Spec.Node
import Test.Spec.Assertions
import Test.Spec.Fixtures
import qualified Test.Spec.Reporter as R

reporterSpec =
  describe "Test" $
    describe "Spec" $
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
