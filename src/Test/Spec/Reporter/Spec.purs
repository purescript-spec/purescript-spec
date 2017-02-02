module Test.Spec.Reporter.Spec (specReporter) where

import Prelude
import Data.Array as Array
import Data.String as String
import Test.Spec.Color as Color
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Spec.Color (colored)
import Test.Spec.Reporter.Base (defaultSummary, defaultReporter)
import Test.Spec.Runner (Reporter)

specReporter
  :: ∀ e. Reporter (console :: CONSOLE | e)
specReporter
  = defaultReporter { indent: 0, numFailures: 0 } update
 where
  update s = case _ of
    Event.Start _ -> s <$ log ""
    Event.Suite name -> modIndent (_ + 1) $ \_ -> _log name
    Event.SuiteEnd   -> modIndent (_ - 1) $ \i -> when (i == 1) (log "")
    Event.Pending name -> s <$ do
      _log $ colored Color.Pending $ "- " <> name
    Event.Pass name speed ms -> s <$ do
      _log $ colored Color.Checkmark "✓︎"
              <> " "
              <> colored Color.Pass name
              <> case speed of
                    Speed.Fast -> ""
                    _ ->
                      let col = Speed.toColor speed
                          label = " (" <> show ms <> "ms)"
                      in colored col label

    Event.Fail name _ _ ->
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
    indent i = String.fromCharArray $ Array.replicate i ' '
