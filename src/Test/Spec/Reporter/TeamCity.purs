-- | Team City reporter, also the one used for intellij
module Test.Spec.Reporter.TeamCity (teamcityReporter, teamcity) where

import Prelude

import Control.Monad.State (get, modify)
import Data.Array ((:))
import Data.Array (intercalate) as Array
import Data.Int (trunc)
import Data.Map.Internal as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Maybe (fromMaybe) as Maybe
import Data.Newtype (unwrap)
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
import Test.Spec.Tree (Path, parentSuite)

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

message :: forall a. String -> ServiceMessage a -> String
message event { name, nodeId, parentNodeId } = teamcity event
  [ "name" /\ name
  , "nodeId" /\ nodeId
  , "parentNodeId" /\ (fromMaybe "0" parentNodeId)
  ]

testCount :: Int -> String
testCount count = teamcity "testCount" [ "count" /\ show count ]

testSuiteStarted :: ServiceMessage () -> String
testSuiteStarted = message "testSuiteStarted"

testSuiteFinished :: ServiceMessage () -> String
testSuiteFinished = message "testSuiteFinished"

testFinishedIn :: WithDuration -> String
testFinishedIn { name, nodeId, parentNodeId, duration } =
  teamcity "testFinished"
    [ "name" /\ name
    , "nodeId" /\ nodeId
    , "duration" /\ show (trunc duration)
    , "parentNodeId" /\ (fromMaybe "0" parentNodeId)
    ]

testFailed :: WithMessage -> String
testFailed { name, nodeId, parentNodeId, message } =
  teamcity "testFailed"
    [ "name" /\ name
    , "nodeId" /\ nodeId
    , "message" /\ message
    , "parentNodeId" /\ (fromMaybe "0" parentNodeId)
    ]

type ServiceMessage x =
  { name :: String
  , nodeId :: String
  , parentNodeId :: Maybe String
  | x
  }

serviceMessage :: String -> Path -> ServiceMessage ()
serviceMessage name' path =
  let
    name = name'
    nodeId = idFromPath path
    parentNodeId = parentSuite path
      <#> _.path
      <#> idFromPath
  in
    { name, nodeId, parentNodeId }

type WithMessage = ServiceMessage (message :: String)
type WithDuration = ServiceMessage (duration :: Number)

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
    name <- get <#> Map.lookup path <#> Maybe.fromMaybe ""
    tellLn $ testSuiteFinished $ serviceMessage name path
  Event.Test _ path name -> do
    tellLn $ message "testStarted" $ serviceMessage name path
  Event.Pending path name -> do
    let attributes = serviceMessage name path
    tellLn $ message "testStarted" attributes
    tellLn $ message "testIgnored" attributes
    tellLn $ message "testFinished" attributes
  Event.TestEnd path name' (Success _ (Milliseconds millies)) -> do
    let
      { name, nodeId, parentNodeId } = serviceMessage name' path
      attributes = { name, nodeId, parentNodeId, duration: millies }
    tellLn $ testFinishedIn attributes
  Event.TestEnd path name' (Failure error) -> do
    let
      { name, nodeId, parentNodeId } = serviceMessage name' path
      attributes = { name, nodeId, parentNodeId, message: show error }
    tellLn $ testFailed attributes
    tellLn $ message "testFinished" attributes
  Event.End _ -> pure unit
  Event.Start count -> tellLn $ testCount count