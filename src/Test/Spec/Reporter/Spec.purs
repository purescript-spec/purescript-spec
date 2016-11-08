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
import Test.Spec.Runner.Event as Event

-- TODO: move these somewhere central (Test.Spec.Console?)
red   = withAttrs [31]
green = withAttrs [32]
blue  = withAttrs [36]

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
    Event.Suite name ->
      let s' = s { indent = s.indent + 1 }
       in s' <$ _log name
    Event.SuiteEnd ->
      let s' = s { indent = s.indent - 1 }
       in s' <$ when (s.indent == 1) (log "")
    Event.Pending name -> s <$ do
      logBlue $ "- " <> name
    Event.Pass name -> s <$ do
      logGreen $ "✓︎ " <> name
    Event.Fail name _ ->
      let s' = s { numFailures = s.numFailures + 1 }
       in s' <$ do
          logRed $ show s'.numFailures <> ") " <> name
    _ -> pure s

    where
    _log msg = log $ indent s.indent <> msg
    logRed   = red   <<< _log
    logGreen = green <<< _log
    logBlue  = blue  <<< _log

    -- TODO: move this somewhere central
    indent i = String.fromCharArray $ Array.replicate i ' '
