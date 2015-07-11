module Test.Spec.RunnerSpec where

import Control.Monad.Aff
import Test.Spec
import Test.Spec.Node
import Test.Spec.Assertions
import Test.Spec.Fixtures

runnerSpec =
  describe "Test" $
    describe "Spec" $
      describe "Runner" do
        it "collects \"it\" and \"pending\" in Describe groups" do
          results <- collect successTest
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success]]]
        it "collects \"it\" and \"pending\" with shared Describes" do
          results <- collect sharedDescribeTest
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success],
                                               Describe "c" [It "also works" Success]]]
        it "supports async" do
          res <- later' 10 $ return 1
          res `shouldEqual` 1
