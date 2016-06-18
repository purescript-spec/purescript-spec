module Test.Spec.RunnerSpec where

import Prelude

import Control.Monad.Aff         (Aff(), later')
import Control.Monad.State.Trans (StateT())

import Test.Spec            (Group(..), Result(..), collect, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Test.Spec.Fixtures (successTest, sharedDescribeTest)

runnerSpec :: forall eff. StateT (Array Group) (Aff eff) Unit
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
          res <- later' 10 $ pure 1
          res `shouldEqual` 1
