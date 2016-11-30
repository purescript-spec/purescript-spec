module Test.Spec.Reporter.Tap (tapReporter) where

import Prelude

import Data.String.Regex as       Regex
import Data.String.Regex.Flags as Regex
import Data.String.Regex          (regex, Regex())
import Data.Either                (fromRight)

import Control.Monad.Eff         (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Spec.Reporter.Base   (BaseReporter, defaultReporter, onUpdate, onSummarize)
import Test.Spec.Console         (write) as Console
import Test.Spec.Color           (colored)
import Test.Spec.Color as        Color
import Test.Spec                 (Group(), Result(..))
import Test.Spec.Summary as      Summary
import Test.Spec.Summary         (Summary(..))
import Test.Spec.Runner.Event as Event

import Partial.Unsafe (unsafePartial)

type TapReporterState = Int
type TapReporterConfig = {}
type TapReporter r = BaseReporter TapReporterConfig TapReporterState r

tapReporter :: ∀ e. TapReporter (Eff (console :: CONSOLE | e))
tapReporter
  = defaultReporter {} 1
      # onUpdate  update
      # onSummarize summarize

 where
  update _ n = case _ of
    Event.Start nTests -> n <$ (log $ "1.." <> show nTests)
    Event.TestEnd -> pure (n + 1)
    Event.Pending name -> n <$ log do
      "ok " <> show n <> " " <> (escTitle name) <> " # SKIP -"
    Event.Pass name _ _ -> n <$ log do
      "ok " <> show n <> " " <> (escTitle name)
    Event.Fail name msg -> n <$ do
      log $ "not ok " <> show n <> " " <> (escTitle name)
      log $ escMsg msg
    _ -> pure n

  summarize _ _ xs =
    case Summary.summarize xs of
      (Count passed failed pending) -> do
        log $ "# tests " <> show (failed + passed + pending)
        log $ "# pass "  <> show (passed + pending)
        log $ "# fail "  <> show failed

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
