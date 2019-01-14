module Test.Spec.Reporter.Spec (specReporter) where

import Prelude

import Data.Array as Array
import Data.String.CodeUnits as CodeUnits
import Effect.Console (log)
import Test.Spec.Color (colored)
import Test.Spec.Color as Color
import Test.Spec.Reporter.Base (defaultSummary, defaultReporter)
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed

specReporter :: Reporter
specReporter
  = defaultReporter { indent: 0, numFailures: 0 } update
 where
  -- TODO coordinate events when multiple test/suites are running in parallel
  update s = case _ of
    Event.Start _ -> s <$ log ""
    Event.Suite path name -> modIndent (_ + 1) $ \_ -> _log name
    Event.SuiteEnd path   -> modIndent (_ - 1) $ \i -> when (i == 1) (log "")
    Event.Pending path name -> s <$ do
      _log $ colored Color.Pending $ "- " <> name
    Event.Pass path name speed ms -> s <$ do
      _log $ colored Color.Checkmark "✓︎"
              <> " "
              <> colored Color.Pass name
              <> case speed of
                    Speed.Fast -> ""
                    _ ->
                      let col = Speed.toColor speed
                          label = " (" <> show ms <> "ms)"
                      in colored col label

    Event.Fail path name _ ->
      let s' = s { numFailures = s.numFailures + 1 }
       in s' <$ (_log $ colored Color.Fail $ show s'.numFailures <> ") " <> name)

    Event.End results -> s <$ defaultSummary results
    _ -> pure s

    where
    _log msg = log $ indent s.indent <> msg
    modIndent f fm =
      let s' = s { indent = f s.indent }
       in s' <$ (fm s'.indent)

    -- TODO: move this somewhere central
    indent i = CodeUnits.fromCharArray $ Array.replicate i ' '
