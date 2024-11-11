module Test.Spec.Runner.Event where

import Prelude

import Data.Tuple.Nested ((/\))
import Test.Spec (Tree)
import Test.Spec.Result (Result)
import Test.Spec.Tree (NumberOfTests, TestLocator)

data Execution = Parallel | Sequential
instance showExecution :: Show Execution where
  show = case _ of
    Parallel -> "Parallel"
    Sequential -> "Sequential"

data Event
  = Start NumberOfTests
  | Suite Execution TestLocator
  | SuiteEnd TestLocator
  | Test Execution TestLocator
  | TestEnd TestLocator Result
  | Pending TestLocator
  | End (Array (Tree String Void Result))

instance showEvent :: Show Event where
  show = case _ of
    Start n -> "Start " <> show n
    Suite e (path /\ name) -> "Suite " <> show e <> show path <> ": " <> name
    SuiteEnd (path /\ name) -> "SuiteEnd " <> show path <> ": " <> name
    Test e (path /\ name) -> "Test " <> show e <> show path <> " " <> name
    TestEnd (path /\ name) res -> "TestEnd " <> show path <> " " <> name <> ": " <> show res
    Pending (path /\ name) -> "Pending " <> show path <> " " <> name
    End results -> "End " <> show results
