module Test.Spec.ReporterSpec where

import Prelude

import Control.Monad.Eff.Exception (error)
import Data.Array                  (concatMap)

import Test.Spec ( Result(..)
                 , Spec
                 , describe
                 , it
                 )
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter   as R
import Test.Spec.Runner     (runSpec)

import Test.Spec.Fixtures ( failureTest
                          , pendingTest
                          , sharedDescribeTest
                          , successTest
                          )

reporterSpec :: forall r. Spec r Unit
reporterSpec =
  describe "Test" $
    describe "Spec" $
      describe "Reporter" do
        it "collapses groups into entries with names" do
          results <- runSpec successTest
          concatMap R.collapse results `shouldEqual` [
              R.Describe ["a", "b"],
              R.It "works" Success
            ]
        it "collapses groups into entries with shared describes" do
          results <- runSpec sharedDescribeTest
          concatMap R.collapse results `shouldEqual` [
              R.Describe ["a", "b"],
              R.It "works" Success,
              R.Describe ["a", "c"],
              R.It "also works" Success
            ]
        it "reports failed tests" do
          results <- runSpec failureTest
          concatMap R.collapse results `shouldEqual` [
            R.It "fails" (Failure (error "1 ≠ 2"))
          ]
        it "reports pending tests" do
          results <- runSpec pendingTest
          concatMap R.collapse results `shouldEqual` [
            R.Pending "is not written yet"
          ]
