module Test.Spec.RunnerSpec where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff (delay)
import Control.Monad.Aff.Console as Console
import Data.Time.Duration (Milliseconds(..))
import Test.Spec (Group(..), Result(..), Spec, describe, it, aroundEach)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Fixtures (itOnlyTest, describeOnlyNestedTest, describeOnlyTest, sharedDescribeTest, successTest)
import Test.Spec.Runner (RunnerEffects, runSpec)

runnerSpec :: ∀ e eff. Spec (RunnerEffects e) Unit
runnerSpec =
  describe "Test" $
    describe "Spec" do
      describe "Runner" do
        it "collects \"it\" and \"pending\" in Describe groups" do
          results <- runSpec successTest
          results `shouldEqual` [Describe false "a" [Describe false "b" [It false "works" Success]]]
        it "collects \"it\" and \"pending\" with shared Describes" do
          results <- runSpec sharedDescribeTest
          results `shouldEqual` [Describe false "a" [Describe false "b" [It false "works" Success],
                                                     Describe false "c" [It false "also works" Success]]]
        it "filters using \"only\" modifier on \"describe\" block" do
          results <- runSpec describeOnlyTest
          results `shouldEqual` [Describe true "a" [Describe false "b" [It false "works" Success],
                                                    Describe false "c" [It false "also works" Success]]]
        it "filters using \"only\" modifier on nested \"describe\" block" do
          results <- runSpec describeOnlyNestedTest
          results `shouldEqual` [Describe true "b" [It false "works" Success]]
        it "filters using \"only\" modifier on \"it\" block" do
          results <- runSpec itOnlyTest
          results `shouldEqual` [It true "works" Success]
        it "supports async" do
          res <- delay (Milliseconds 10.0) *> pure 1
          res `shouldEqual` 1

      describe "aroundEach" do
        aroundEach
          ((pure 10) :: Aff _ Int)
          ((Console.log <<< show) :: Int -> Aff _ Unit) do
            it "should pass result to \"it\" (1)" do
              10 `shouldEqual` 10
