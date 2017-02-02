module Test.Spec.Runner.Event where

import Prelude
import Data.Maybe (Maybe)
import Test.Spec (Group, Result)
import Test.Spec.Speed (Speed)

type Message = String
type Name = String
type Duration = Int
type NumberOfTests = Int
type Stack = String

data Event
  = Start NumberOfTests
  | Suite String
  | Test
  | TestEnd
  | SuiteEnd
  | Fail Name Message (Maybe Stack)
  | Pass Name Speed Duration
  | Pending String
  | End (Array (Group Result))

instance showEvent :: Show Event where
  show =
    case _ of
      Start n -> "Start " <> show n
      Suite name ->  "Suite " <> name
      Test -> "Test"
      TestEnd -> "TestEnd"
      SuiteEnd -> "SuiteEnd"
      Fail name msg _ -> "Fail " <> name <> ": " <> msg
      Pass name speed duration -> "Pass " <> name <> " "
                                  <> show speed <> " "
                                  <> show duration
      Pending name -> "Pending " <> name
      End results -> "End " <> show results
