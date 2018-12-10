module Test.Spec.RunnerSpec where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Class.Console (log)
import Test.Spec (Group(..), Result(..), Spec, describe, describePar, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Fixtures (itOnlyTest, describeOnlyNestedTest, describeOnlyTest, sharedDescribeTest, successTest)
import Test.Spec.Runner (runSpec)

runnerSpec :: Spec Unit
runnerSpec =
  describe "Test" $
    describe "Spec" $
      describePar "Runner" do
        it "collects \"it\" and \"pending\" in Describe groups" do
          log "start 1"
          delay $ Milliseconds $ 1000.0 + 300.0 * 1.0
          log "done 1"
          results <- runSpec successTest
          results `shouldEqual` [Describe {only: false, parallel: false} "a" [Describe {only: false, parallel: false} "b" [It false "works" Success]]]
        it "collects \"it\" and \"pending\" with shared Describes" do
          log "start 2"
          delay $ Milliseconds $ 1000.0 + 300.0 * 2.0
          log "done 2"
          results <- runSpec sharedDescribeTest
          results `shouldEqual` [Describe {only: false, parallel: false} "a" [Describe {only: false, parallel: false} "b" [It false "works" Success],
                                                     Describe {only: false, parallel: false} "c" [It false "also works" Success]]]
        it "filters using \"only\" modifier on \"describe\" block" do
          log "start 3"
          delay $ Milliseconds $ 1000.0 + 300.0 * 3.0
          log "done 3"
          results <- runSpec describeOnlyTest
          results `shouldEqual` [Describe {only: true, parallel: false} "a" [Describe {only: false, parallel: false} "b" [It false "works" Success],
                                                    Describe {only: false, parallel: false} "c" [It false "also works" Success]]]
        it "filters using \"only\" modifier on nested \"describe\" block" do
          log "start 4"
          delay $ Milliseconds $ 1000.0 + 300.0 * 4.0
          log "done 4"
          results <- runSpec describeOnlyNestedTest
          results `shouldEqual` [Describe {only: true, parallel: false} "b" [It false "works" Success]]
        it "filters using \"only\" modifier on \"it\" block" do
          log "start 5"
          delay $ Milliseconds $ 1000.0 + 300.0 * 5.0
          log "done 5"
          results <- runSpec itOnlyTest
          results `shouldEqual` [It true "works" Success]
        it "supports async" do
          log "start 6"
          delay $ Milliseconds $ 1000.0 + 300.0 * 6.0
          log "done 6"
          res <- delay (Milliseconds 10.0) *> pure 1
          res `shouldEqual` 1
