module Test.Spec.Reporter.Spec (specReporter) where

import Prelude

import Data.Foldable  (intercalate)
import Data.String as String
import Data.Array as  Array

import Control.Monad.Eff              (Eff)
import Control.Monad.Eff.Console      (CONSOLE, log)
import Control.Monad.Eff.Exception as Error

import Test.Spec.Reporter.Base   (BaseReporter, defaultReporter, onUpdate)
import Test.Spec                 (Group, Result(..))
import Test.Spec.Console         (withAttrs)
import Test.Spec.Color           (colored)
import Test.Spec.Color as        Color
import Test.Spec.Runner.Event as Event

type SpecReporterStateObj = {
  indent :: Int
, numFailures :: Int
}

initialState :: SpecReporterStateObj
initialState = { indent: 0, numFailures: 0 }

specReporter :: ∀ e. BaseReporter SpecReporterStateObj (Eff (console :: CONSOLE | e))
specReporter = defaultReporter initialState # onUpdate update

 where
  update s = case _ of
    Event.Start -> s <$ do
      log ""
    Event.Suite name -> modIndent (_ + 1) $ \_ -> _log name
    Event.SuiteEnd   -> modIndent (_ - 1) $ \i -> when (i == 1) (log "")
    Event.Pending name -> s <$ do
      _log $ colored Color.Pending $ "- " <> name
    Event.Pass name    -> s <$ do
      _log $ colored Color.Checkmark "✓︎" <> " " <> colored Color.Pass name
    Event.Fail name _ ->
      let s' = s { numFailures = s.numFailures + 1 }
       in s' <$ (_log $ colored Color.Fail $ show s'.numFailures <> ") " <> name)
    _ -> pure s

    where
    _log msg = log $ indent s.indent <> msg
    modIndent f fm =
      let s' = s { indent = f s.indent }
       in s' <$ (fm s'.indent)

    -- TODO: move this somewhere central
    indent i = String.fromCharArray $ Array.replicate i ' '
