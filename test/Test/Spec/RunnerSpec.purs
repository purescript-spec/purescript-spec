module Test.Spec.RunnerSpec where

import Prelude

import Control.Monad.Aff         (later')

import Test.Spec            ( Group(..)
                            , Result(..)
                            , Spec
                            , describe
                            , it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner     (runSpec)

import Test.Spec.Fixtures (successTest, sharedDescribeTest)

runnerSpec :: forall r. Spec r Unit
runnerSpec =
  describe "Test" $
    describe "Spec" $
      describe "Runner" do
        it "collects \"it\" and \"pending\" in Describe groups" do
          results <- runSpec successTest
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success]]]
        it "collects \"it\" and \"pending\" with shared Describes" do
          results <- runSpec sharedDescribeTest
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success],
                                               Describe "c" [It "also works" Success]]]
        it "supports async" do
          res <- later' 10 $ pure 1
          res `shouldEqual` 1
