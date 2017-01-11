module Test.Spec.ReporterSpec where

import Prelude
import Control.Monad.Eff.Exception (error)
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Test.Spec (Result(..), Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Fixtures (failureTest, pendingTest, sharedDescribeTest, duplicatedDescribeTest, successTest)
import Test.Spec.Reporter (collapseAll, Entry(..))
import Test.Spec.Runner (RunnerEffects, runSpec)

reporterSpec :: ∀ e. Spec (RunnerEffects e) Unit
reporterSpec =
  describe "Test" $
    describe "Spec" $
      describe "Reporter" $ do
        it "collapses groups into entries with names" do
          results <- runSpec successTest
          collapseAll results
            `shouldEqual`
            (fromFoldable [Tuple ["a", "b"] [It "works" Success]])
        it "collapses groups into entries with shared describes" do
          results <- runSpec sharedDescribeTest
          collapseAll results
            `shouldEqual`
            (fromFoldable
             [ (Tuple ["a", "b"] [It "works" Success])
             , (Tuple ["a", "c"] [It "also works" Success])
             ])
        it "collapses groups into entries with duplicated describes" do
          results <- runSpec duplicatedDescribeTest
          collapseAll results
            `shouldEqual`
            (fromFoldable
             [ (Tuple ["a", "b", "c"] [It "first" Success
                                      , It "second" Success])
             ])
        it "reports failed tests" do
          results <- runSpec failureTest
          collapseAll results
            `shouldEqual`
            (fromFoldable [(Tuple [] [It "fails" (Failure (error "1 ≠ 2"))])])
        it "reports pending tests" do
          results <- runSpec pendingTest
          collapseAll results
            `shouldEqual`
            (fromFoldable [(Tuple [] [Pending "is not written yet"])])
