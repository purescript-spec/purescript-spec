module Test.Spec.Reporter.Tap (tapReporter) where

import Prelude

import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(Pattern), joinWith, split)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Effect.Console (log)
import Effect.Exception as Error
import Partial.Unsafe (unsafePartial)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary

type TapReporterState = Int

tapReporter :: Reporter
tapReporter =
 defaultReporter 1 update
 where
  update n = case _ of
    Event.Start nTests -> n <$ (log $ "1.." <> show nTests)
    Event.Pending _ name -> n <$ log do
      "ok " <> show n <> " " <> (escTitle name) <> " # SKIP -"
    Event.Pass _ name _ _ -> n + 1 <$ log do
      "ok " <> show n <> " " <> (escTitle name)
    Event.Fail _ name err -> n + 1 <$ do
      log $ "not ok " <> show n <> " " <> (escTitle name)
      log $ escMsg $ Error.message err
      case Error.stack err of
        Nothing -> pure unit
        Just s  -> log $ joinWith "\n" (append "    " <$> split (Pattern "\n") s)
    Event.End results -> do
      case Summary.summarize results of
        (Count {passed, failed, pending}) -> do
          log $ "# tests " <> show (failed + passed + pending)
          log $ "# pass "  <> show (passed + pending)
          log $ "# fail "  <> show failed
      pure n
    _ -> pure n

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
