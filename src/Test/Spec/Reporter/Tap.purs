module Test.Spec.Reporter.Tap (tapReporter) where

import Prelude

import Control.Monad.State (get, modify_)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(Pattern), joinWith, split)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Effect.Exception as Error
import Partial.Unsafe (unsafePartial)
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary

type TapReporterState = Int

tapReporter :: Reporter
tapReporter = defaultReporter 1 case _ of
  Event.Start nTests ->
    tellLn $ "1.." <> show nTests
  Event.Pending _ name -> do
    n <- get
    tellLn $ "ok " <> show n <> " " <> (escTitle name) <> " # SKIP -"
    modify_ (_ + 1)
  Event.TestEnd _ name (Success _ _) -> do
    n <- get
    tellLn $ "ok " <> show n <> " " <> (escTitle name)
    modify_ (_ + 1)
  Event.TestEnd _ name (Failure err) -> do
    n <- get
    tellLn $ "not ok " <> show n <> " " <> (escTitle name)
    tellLn $ escMsg $ Error.message err
    case Error.stack err of
      Nothing -> pure unit
      Just s  -> tellLn $ joinWith "\n" (append "    " <$> split (Pattern "\n") s)
    modify_ (_ + 1)
  Event.End results -> do
    let (Count {passed, failed, pending}) = Summary.summarize results
    tellLn $ "# tests " <> show (failed + passed + pending)
    tellLn $ "# pass " <> show (passed + pending)
    tellLn $ "# fail " <> show failed
  _ -> pure unit

-- create a TAP-safe title
escMsg :: String -> String
escMsg =
  let rex = unsafePartial $ fromRight $ regex "^" $ Regex.parseFlags "gm"
    in Regex.replace rex "  "

-- create a TAP-safe error msg
escTitle :: String -> String
escTitle =
  let rex = unsafePartial $ fromRight $ regex "#" $ Regex.parseFlags "g"
    in Regex.replace rex ""
