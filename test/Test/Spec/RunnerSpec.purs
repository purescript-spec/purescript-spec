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

import Test.Spec.Fixtures (successTest, sharedDescribeTest, onlyTest)

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
        it "collects \"it\" and \"pending\" with \"only\" modifier" do
          results <- runSpec onlyTest
          results `shouldEqual` [Describe true "a" [Describe false "b" [It false "works" Success],
                                                    Describe false "c" [It true "also works" Success]]]
        itOnly "supports async" do
          res <- later' 10 $ pure 1
          res `shouldEqual` 1
