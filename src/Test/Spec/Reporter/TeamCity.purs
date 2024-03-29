-- | Team City reporter, also the one used for intellij
module Test.Spec.Reporter.TeamCity (teamcityReporter, teamcity) where

import Prelude

import Control.Monad.State (get, modify)
import Data.Foldable (for_)
import Data.Int (trunc)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Data.Time.Duration (Milliseconds(..))
import Effect.Exception (Error)
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Tree (Path, parentSuite)
import Data.Array (intercalate) as Array
import Effect.Exception (message, stack) as Error
import Test.Spec.Runner.Event (Event(..)) as Event
import Data.Map.Internal as Map
import Data.String.Regex (replace') as Regex
import Data.String.Regex.Flags (global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.String.CodeUnits (contains, drop, dropRight, dropWhile, take, takeWhile) as String
import Data.String.Common (replaceAll) as String

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

teamcity :: forall a. String -> ServiceMessage a -> String
teamcity = teamcity' ""

teamcity' :: forall a. String -> String -> ServiceMessage a -> String
teamcity' rest event { name, nodeId, parentNodeId } = "##teamcity["
  <> event
  <> "name" := name
  <> "nodeId" := nodeId
  <> "parentNodeId" := fromMaybe "0" parentNodeId
  <> rest
  <> "]"

property :: String -> String -> String
property key value = " " <> key <> "='" <> escape value <> "'"

infix 7 property as :=

testCount :: Int -> String
testCount count = "##teamcity[testCount count='" <> show count <> "']"

testSuiteStarted :: forall a. ServiceMessage a -> String
testSuiteStarted = teamcity' "" "testSuiteStarted"

testSuiteFinished :: forall a. ServiceMessage a -> String
testSuiteFinished = teamcity' "" "testSuiteFinished"

testStarted :: forall a. ServiceMessage a -> String
testStarted = teamcity' "" "testStarted"

testIgnored :: forall a. ServiceMessage a -> String
testIgnored = teamcity' "" "testIgnored"

testFinished :: forall a. ServiceMessage a -> String
testFinished = teamcity' "" "testFinished"

testFinishedIn :: WithDuration -> String
testFinishedIn d = teamcity' ("duration" := (show $ trunc d.duration)) "testFinished" d

testFailed :: WithMessage -> Error -> String
testFailed d e =
  let
    message = Error.message e
    isEquals = String.contains (Pattern "≠") message
  in
    if isEquals then
      let
        readString string = string
          # String.replaceAll (Pattern "\\n") (Replacement "\n")
          # String.drop 1
          # String.dropRight 1
        read s =
          if String.take 1 s == "\"" then readString s
          else s

        expected = message
          # String.takeWhile (_ /= '≠')
          # String.dropRight 1
          # read

        actual = message
          # String.dropWhile (_ /= '≠')
          # String.drop 2
          # String.replaceAll (Pattern "\\n") (Replacement "\n")
          # read
      in
        teamcity'
          ( "type" := "comparisonFailure"
              <> "details" := (Error.stack e # fromMaybe "")
              <> "message" := message
              <> "expected" := expected
              <> "actual" := actual
          )
          "testFailed"
          d
    else teamcity' ("message" := d.message) "testFailed" d

type ServiceMessage x =
  { name :: String
  , nodeId :: String
  , parentNodeId :: Maybe String
  | x
  }

type WithMessage = ServiceMessage (message :: String)
type WithDuration = ServiceMessage (duration :: Number)

serviceMessage :: String -> Path -> ServiceMessage ()
serviceMessage name path =
  let
    nodeId = idFromPath path
    parentNodeId = parentSuite path
      <#> _.path
      <#> idFromPath
  in
    { name, nodeId, parentNodeId }

withDuration :: Number -> ServiceMessage () -> WithDuration
withDuration duration { name, nodeId, parentNodeId } =
  { name, nodeId, parentNodeId, duration }

withMessage :: String -> ServiceMessage () -> WithMessage
withMessage message { name, nodeId, parentNodeId } =
  { name, nodeId, parentNodeId, message }

idFromPath :: Path -> String
idFromPath path = path
  <#> unwrap
  <#> (\{ index, name } -> show index <> ":" <> fromMaybe "" name)
  # Array.intercalate ","

teamcityReporter :: Reporter
teamcityReporter = defaultReporter Map.empty case _ of
  Event.Suite _ path name -> do
    void $ modify $ Map.insert path name
    tellLn $ testSuiteStarted $ serviceMessage name path
  Event.SuiteEnd path -> do
    maybeName <- get <#> Map.lookup path
    for_ maybeName \name -> do
      tellLn $ testSuiteFinished $ serviceMessage name path
  Event.Test _ path name -> do
    tellLn $ testStarted $ serviceMessage name path
  Event.Pending path name -> do
    let attributes = serviceMessage name path
    tellLn $ testStarted attributes
    tellLn $ testIgnored attributes
    tellLn $ testFinished attributes
  Event.TestEnd path name (Success _ (Milliseconds millies)) ->
    tellLn $ testFinishedIn
      ( serviceMessage name path
          # withDuration millies
      )
  Event.TestEnd path name (Failure error) -> do
    let attributes = serviceMessage name path # withMessage (show error)
    tellLn $ testFailed attributes error
    tellLn $ testFinished attributes
  Event.End _ -> pure unit
  Event.Start count -> tellLn $ testCount count
