-- | Team City reporter, also the one used for intellij
module Test.Spec.Reporter.TeamCity (teamcityReporter, teamcity) where

import Prelude

import Control.Monad.State (get, modify)
import Data.Array ((:))
import Data.Array (intercalate) as Array
import Data.Int (trunc)
import Data.Map.Internal as Map
import Data.Maybe (fromMaybe) as Maybe
import Data.String.Regex (replace') as Regex
import Data.String.Regex.Flags (global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Event(..)) as Event

escape :: String -> String
escape = Regex.replace'
  (Regex.unsafeRegex "(?:[|\n\r'\\[\\]])" $ Regex.global)
  \match _ -> case match of
    "|" -> "||"
    "\n" -> "|n"
    "\r" -> "|r"
    "[" -> "|["
    "]" -> "|]"
    "'" -> "|'"
    _ -> ""

teamcity :: String -> Array (Tuple String String) -> String
teamcity name properties = "##teamcity[" <> body <> "]"
  where
  body = name : (properties <#> renderProperty) # Array.intercalate " "
  renderProperty (key /\ value) = key <> "='" <> escape value <> "'"

testCount :: Int -> String
testCount count = teamcity "testCount" [ "count" /\ show count ]

testSuiteStarted :: Array (Tuple String String) -> String
testSuiteStarted = teamcity "testSuiteStarted"

testSuiteFinished :: Array (Tuple String String) -> String
testSuiteFinished = teamcity "testSuiteFinished"

testStarted :: Array (Tuple String String) -> String
testStarted = teamcity "testStarted"

testIgnored :: Array (Tuple String String) -> String
testIgnored = teamcity "testIgnored"

testFinished :: Array (Tuple String String) -> String
testFinished = teamcity "testFinished"

testFailed :: Array (Tuple String String) -> String
testFailed = teamcity "testFailed"

teamcityReporter :: Reporter
teamcityReporter = defaultReporter Map.empty case _ of
  Event.Suite _ path name -> do
    void $ modify $ Map.insert path name
    tellLn $ testSuiteStarted [ "name" /\ name ]
  Event.SuiteEnd path -> do
    name <- get <#> Map.lookup path <#> Maybe.fromMaybe ""
    tellLn $ testSuiteFinished [ "name" /\ name ]
  Event.Test _ _ name ->
    tellLn $ testStarted [ "name" /\ name ]
  Event.Pending _ name -> do
    tellLn $ testStarted [ "name" /\ name ]
    tellLn $ testIgnored [ "name" /\ name ]
    tellLn $ testFinished [ "name" /\ name ]
  Event.TestEnd _ name (Success _ (Milliseconds millies)) -> do
    tellLn $ testFinished
      [ "name" /\ name
      , "duration" /\ show (trunc millies)
      ]
  Event.TestEnd _ name (Failure error) -> do
    tellLn $ testFailed
      [ "name" /\ name
      , "message" /\ show error
      ]
    tellLn $ testFinished [ "name" /\ name ]
  Event.End _ -> pure unit
  Event.Start count -> tellLn $ testCount count
