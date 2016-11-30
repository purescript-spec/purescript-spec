module Test.Spec.Runner.Event where

import Prelude
import Data.Generic (class Generic, gShow)
import Test.Spec.Speed (Speed)

type Message = String
type Name = String
type Duration = Int
type NumberOfTests = Int

data Event
  = Start NumberOfTests
  | Suite String
  | Test
  | TestEnd
  | SuiteEnd
  | Fail Name Message
  | Pass Name Speed Duration
  | Pending String
  | End

derive instance genericEvent :: Generic Event

instance showEvent :: Show Event
  where show = gShow
