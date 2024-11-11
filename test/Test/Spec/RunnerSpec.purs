module Test.Spec.RunnerSpec where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay)
import Test.Spec (Item(..), Spec, SpecT, Tree(..), collect, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Fixtures (itOnlyTest, describeOnlyNestedTest, describeOnlyTest, sharedDescribeTest, successTest)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (TreeFilter(..), defaultConfig, evalSpecT)
import Test.Spec.Tree (annotatedWithPaths, filterTrees, mapTreeAnnotations, parentSuiteName)

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
          res <- un Identity $ evalSpecT config [] $
            describe "A test" do
              it "fails" $ 1 `shouldEqual` 2
              it "also fails" $ 1 `shouldEqual` 2
              it "succeeds" $ 1 `shouldEqual` 1
              it "fails again" $ 1 `shouldEqual` 2

          (map mapSuccess <$> res) `shouldEqual`
            [ Node (Left "A test")
              [ Leaf "fails" (Just false)
              , Leaf "also fails" Nothing
              , Leaf "succeeds" Nothing
              , Leaf "fails again" Nothing
              ]
            ]

        it "supports tree filtering" do
          let config = defaultConfig
                { exit = false
                , filterTree = TreeFilter \trees -> trees
                    # annotatedWithPaths                     -- Give every node a path
                    # filterTrees (\(path /\ name) _ ->     -- Use the path for filtering nodes
                        parentSuiteName path <> [name]
                        # Str.joinWith " "
                        # Str.contains (Str.Pattern "a b")  -- Leave only nodes that have "a b" in their path somewhere
                      )
                    <#> mapTreeAnnotations snd              -- Drop the paths from the tree
                }

          res <- un Identity $ evalSpecT config [] do
            describe "aaa" do
              it "bbb" $ pure unit
              it "ccc" $ fail "boom"
            describe "foo" do
              it "bbb" $ fail "boom"
              it "ccc" $ fail "boom"
              it "cccb aaa" $ pure unit
              describe "bara" do
                it "ccc" $ fail "boom"
                it "bob" $ pure unit
            describe "baa" $
              describe "baa" $
                it "foo" $ pure unit

          (map mapSuccess <$> res) `shouldEqual`
            [ Node (Left "aaa")
              [ Leaf "bbb" (Just true)
              ]
            , Node (Left "foo")
              [ Node (Left "bara")
                [ Leaf "bob" (Just true)
                ]
              ]
            , Node (Left "baa")
              [ Node (Left "baa")
                [ Leaf "foo" (Just true)
                ]
              ]
            ]

  where
    runSpecFocused :: SpecT Identity Unit Identity Unit -> Array (Tree String Unit Boolean)
    runSpecFocused t = un Identity (collect t) <#> (bimap (const unit) (un Item >>> _.isFocused))

    mapSuccess (Success _ _) = true
    mapSuccess (Failure _) = false
