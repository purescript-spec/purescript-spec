module Test.Spec.RunnerSpec where

import Prelude

import Control.Monad.Aff         (later')

import Test.Spec            ( Group(..)
                            , Result(..)
                            , Spec
                            , describe
                            , it
                            , itOnly
                            )
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner     (runSpec)

import Test.Spec.Fixtures

runnerSpec :: forall r. Spec r Unit
runnerSpec =
  describe "Test" $
    describe "Spec" $
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
          res <- later' 10 $ pure 1
          res `shouldEqual` 1
