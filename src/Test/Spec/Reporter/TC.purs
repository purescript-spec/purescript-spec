-- | Team City reporter, also the one used for intellij
module Test.Spec.Reporter.TC (tcReporter) where

import Prelude

import Control.Monad.State (get, put)
import Data.Array (last) as Array
import Data.Int (trunc)
import Data.Maybe (fromMaybe) as Maybe
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Event(..)) as Event
import Test.Spec.Tree as Tree

message :: forall a. Show a => a -> String
message error = show error
    # (String.replaceAll (String.Pattern "\n") (String.Replacement "|n"))

nameFromPath :: Tree.Path -> String
nameFromPath path = Tree.parentSuiteName path
    # Array.last
    # Maybe.fromMaybe ""


tcReporter :: Reporter
tcReporter = defaultReporter "" case _ of
  Event.Suite _ _ name -> do
    put name
    tellLn $ "##teamcity[testSuiteStarted name='" <> name <> "']"
  Event.SuiteEnd path -> do
    name <- get
    put $ nameFromPath path
    tellLn $ "##teamcity[testSuiteFinished name='" <> name <> "']"
  Event.Test _ _ name ->
    tellLn $ "##teamcity[testStarted name='" <> name <> "']"
  Event.Pending _ name -> do
    tellLn $ "##teamcity[testStarted name='" <> name <> "']"
    tellLn $ "##teamcity[testIgnored name='" <> name <> "']"
    tellLn $ "##teamcity[testFinished name='" <> name <> "']"
  Event.TestEnd _ name (Success _ (Milliseconds millies)) -> do
    tellLn $ "##teamcity[testFinished name='" <> name <> "' duration='" <> show (trunc millies) <> "']"
  Event.TestEnd _ name (Failure error) -> do
    tellLn $ "##teamcity[testFailed name='" <> name <> "' message='" <> message error <> "']"
    tellLn $ "##teamcity[testFinished name='" <> name <> "']"
  Event.End _ -> tellLn ""
  _ -> pure unit
