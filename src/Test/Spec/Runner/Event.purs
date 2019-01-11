module Test.Spec.Runner.Event where

import Prelude

import Data.Maybe (Maybe)
import Test.Spec (Result, Tree)
import Test.Spec.Speed (Speed)

type Message = String
type Name = String
type Duration = Int
type NumberOfTests = Int
type Stack = String

newtype PathItem = PathItem { name :: Maybe String, index :: Int}

derive newtype instance showIdTerm :: Show PathItem
derive newtype instance eqIdTerm :: Eq PathItem

type Path = Array PathItem

data Event
  = Start NumberOfTests
  | Suite Path Name
  | Test Path
  | TestEnd Path
  | SuiteEnd Path
  | Fail Path Name Message (Maybe Stack)
  | Pass Path Name Speed Duration
  | Pending Path Name
  | End (Array (Tree Void Result))

instance showEvent :: Show Event where
  show =
    case _ of
      Start n -> "Start " <> show n
      Suite path name -> "Suite " <> show path <> ": " <> name
      Test path -> "Test " <> show path
      TestEnd path -> "TestEnd " <> show path
      SuiteEnd path -> "SuiteEnd " <> show path
      Fail path name msg _ -> "Fail " <> show path <> " " <> name <> ": " <> msg
      Pass path name speed duration -> "Pass " <> show path <> " " <> name <> " "
                                  <> show speed <> " "
                                  <> show duration
      Pending path name -> "Pending " <> show path <> " " <> name
      End results -> "End " <> show results
