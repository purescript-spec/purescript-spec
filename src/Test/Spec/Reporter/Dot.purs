module Test.Spec.Reporter.Dot (dotReporter) where

import Prelude

import Data.Foldable     (intercalate)
import Data.String as    String
import Data.Array as     Array
import Data.Tuple        (Tuple)
import Data.Tuple.Nested ((/\))

import Control.Monad.Eff              (Eff)
import Control.Monad.Eff.Console      (CONSOLE, log)
import Control.Monad.Eff.Exception as Error

import Test.Spec.Reporter.Base   (BaseReporter, defaultReporter, onUpdate)
import Test.Spec                 (Group, Result(..))
import Test.Spec.Console         (withAttrs)
import Test.Spec.Console         (write) as Console
import Test.Spec.Color           (colored)
import Test.Spec.Color as        Color
import Test.Spec.Runner.Event as Event

type DotReporterState = Tuple Int Int

dotReporter
  :: Int -- the width of the block of dots (i.e. how many tests per line?)
  -> ∀ e. BaseReporter DotReporterState (Eff (console :: CONSOLE | e))
dotReporter w = defaultReporter (w /\ -1)
  # onUpdate update

 where
  update s@(w /\ n) = case _ of
    Event.Pass _    -> wrap $ Console.write (colored Color.Pending ".")
    Event.Pending _ -> wrap $ Console.write (colored Color.Pass    ",")
    Event.Fail  _ _ -> wrap $ Console.write (colored Color.Fail    "!")
    Event.End       -> s   <$ Console.write "\n"
    _               -> pure s

    where
    wrap action =
      let n' = n + 1
       in (w /\ n') <$ do
            when (n' `mod` w == 0) (Console.write "\n")
            action

