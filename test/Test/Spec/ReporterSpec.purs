module Test.Spec.ReporterSpec where

import Prelude

import Control.Monad.Aff           (Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.State.Trans   (StateT())
import Data.Array                  (concatMap)

import           Test.Spec            ( Group()
                                      , Result(..)
                                      , collect
                                      , describe
                                      , it
                                      )
import           Test.Spec.Assertions (shouldEqual)
import qualified Test.Spec.Reporter   as R

import Test.Spec.Fixtures ( failureTest
                          , pendingTest
                          , sharedDescribeTest
                          , successTest
                          )

reporterSpec :: forall eff. StateT (Array Group) (Aff eff) Unit
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
        it "reports failed tests" do
          results <- collect failureTest
          concatMap R.collapse results `shouldEqual` [
            R.It "fails" (Failure (error "1 ≠ 2"))
          ]
        it "reports pending tests" do
          results <- collect pendingTest
          concatMap R.collapse results `shouldEqual` [
            R.Pending "is not written yet"
          ]
