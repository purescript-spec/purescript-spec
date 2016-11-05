module Test.Spec.Runner.Event where

import Prelude
import Data.Generic

data Event
  = Start
  | Suite String
  | Test
  | TestEnd
  | SuiteEnd
  | Fail String String
  | Pass String
  | Pending String
  | End

derive instance genericEvent :: Generic Event

instance showEvent :: Show Event
  where show = gShow
