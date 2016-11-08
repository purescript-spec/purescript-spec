module Test.Spec.Reporter.Dot (dotReporter) where

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

type DotReporterState = Int

initialState :: DotReporterState
initialState = -1

dotReporter :: ∀ e. BaseReporter DotReporterState (Eff (console :: CONSOLE | e))
dotReporter = defaultReporter initialState # onUpdate update

 where
  update s = case _ of
    _ -> pure s

    -- where
    -- _log msg = log $ indent s.indent <> msg
    -- logRed   = red   <<< _log
    -- logGreen = green <<< _log
    -- logBlue  = blue  <<< _log
    --
    -- -- TODO: move this somewhere central
    -- indent i = String.fromCharArray $ Array.replicate i ' '

