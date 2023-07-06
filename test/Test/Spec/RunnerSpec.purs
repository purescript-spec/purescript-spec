module Test.Spec.RunnerSpec where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Test.Spec (Item(..), Spec, SpecT, Tree(..), collect, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Fixtures (itOnlyTest, describeOnlyNestedTest, describeOnlyTest, sharedDescribeTest, successTest)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (defaultConfig, runSpecT)

runnerSpec :: Spec Unit
runnerSpec =
  describe "Test" $
    describe "Spec" $
      describe "Runner" do
        it "collects \"it\" and \"pending\" in Describe groups" do
          runSpecFocused successTest `shouldEqual`
            [ Node (Left "a")
              [ Node (Left "b") [ Leaf "works" $ Just false ]
              ]
            ]
        it "collects \"it\" and \"pending\" with shared Describes" do
          runSpecFocused sharedDescribeTest `shouldEqual`
            [ Node (Left "a")
              [ Node (Left "b") [ Leaf "works" $ Just false ]
              , Node (Left "c") [ Leaf "also works" $ Just false ]
              ]
            ]
        it "filters using \"only\" modifier on \"describe\" block" do
          runSpecFocused describeOnlyTest `shouldEqual`
            [ Node (Left "a")
              [ Node (Left "b") [ Leaf "works" $ Just true ]
              , Node (Left "c") [ Leaf "also works" $ Just true ]
              ]
            ]
        it "filters using \"only\" modifier on nested \"describe\" block" do
          runSpecFocused describeOnlyNestedTest `shouldEqual`
            [ Node (Left "a")
              [ Node (Left "b") [ Leaf "works" $ Just true ]
              ]
            ]
        it "filters using \"only\" modifier on \"it\" block" do
          runSpecFocused itOnlyTest `shouldEqual`
            [ Node (Left "a")
              [ Node (Left "b") [ Leaf "works" $ Just true ]
              ]
            ]
        it "supports async" do
          res <- delay (Milliseconds 10.0) *> pure 1
          res `shouldEqual` 1
        it "supports fail-fast" do
          let config = defaultConfig { exit = false, failFast = true }
          res <- un Identity $ runSpecT config [] $
            describe "A test" do
              it "fails" $ 1 `shouldEqual` 2
              it "also fails" $ 1 `shouldEqual` 2
              it "succeeds" $ 1 `shouldEqual` 1
              it "fails again" $ 1 `shouldEqual` 2

          case res of
            [Leaf "A test fails" (Just (Failure _))] ->
              pure unit
            unexpectedResult ->
              fail $ "Got unexpected result: " <> show unexpectedResult

  where
    runSpecFocused :: SpecT Identity Unit Identity Unit -> Array (Tree Unit Boolean)
    runSpecFocused t = un Identity (collect t) <#> (bimap (const unit) (un Item >>> _.isFocused))
