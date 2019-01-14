module Test.Spec.Runner.Event where

import Prelude

import Effect.Exception (Error)
import Effect.Exception as Error
import Test.Spec (Result, Tree)
import Test.Spec.Speed (Speed)
import Test.Spec.Tree (Path)

type Message = String
type Name = String
type Duration = Int
type NumberOfTests = Int
type Stack = String

data Event
  = Start NumberOfTests
  | Suite Path Name
  | SuiteEnd Path
  | Test Path Name
  | Fail Path Name Error
  | Pass Path Name Speed Duration
  | Pending Path Name
  | End (Array (Tree Void Result))

instance showEvent :: Show Event where
  show =
    case _ of
      Start n -> "Start " <> show n
      Suite path name -> "Suite " <> show path <> ": " <> name
      Test path name -> "Test " <> show path <> " " <> name
      SuiteEnd path -> "SuiteEnd " <> show path
      Fail path name err -> "Fail " <> show path <> " " <> name <> ": " <> Error.message err
      Pass path name speed duration -> "Pass " <> show path <> " " <> name <> " "
                                  <> show speed <> " "
                                  <> show duration
      Pending path name -> "Pending " <> show path <> " " <> name
      End results -> "End " <> show results
