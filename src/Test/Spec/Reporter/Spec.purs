module Test.Spec.Reporter.Spec (specReporter) where

import Prelude

import Control.Monad.State (get, lift, modify, modify_)
import Data.Array as Array
import Data.String.CodeUnits as CodeUnits
import Effect.Console (log)
import Test.Spec.Color (colored)
import Test.Spec.Color as Color
import Test.Spec.Console (logWriter)
import Test.Spec.Reporter.Base (defaultSummary, defaultReporter)
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed

-- TODO coordinate events when multiple test/suites are running in parallel
specReporter :: Reporter
specReporter = defaultReporter { indent: 0, numFailures: 0 } case _ of
  Event.Suite path name -> do
    modify_ $ onIndent (_ + 1)
    _log name
  Event.SuiteEnd path -> do
    s <- modify $ onIndent (_ - 1)
    when (s.indent == 1) do
      lift $ log ""
  Event.Pending path name -> do
    _log $ colored Color.Pending $ "- " <> name
  Event.Pass path name speed ms -> do
    let
      speedDetails = case speed of
        Speed.Fast -> ""
        _ -> colored (Speed.toColor speed) $ " (" <> show ms <> "ms)"
    _log $ colored Color.Checkmark "✓︎" <> " " <> colored Color.Pass name <> speedDetails
  Event.Fail path name _ -> do
    s <- modify \s -> s{ numFailures = s.numFailures + 1 }
    _log $ colored Color.Fail $ show s.numFailures <> ") " <> name
  Event.End results -> lift $ logWriter (defaultSummary results)
  Event.Start _ -> pure unit
  Event.Test _ _ -> pure unit

  where
    onIndent f s = s { indent = f s.indent }
    _log msg = do
      s <- get
      lift $ log $ indent s.indent <> msg

    -- TODO: move this somewhere central
    indent i = CodeUnits.fromCharArray $ Array.replicate i ' '
