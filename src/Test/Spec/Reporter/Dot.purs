module Test.Spec.Reporter.Dot (dotReporter) where

import Prelude

import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import Test.Spec.Color (colored)
import Test.Spec.Color as Color
import Test.Spec.Console (logWriter, tellLn)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed

type DotReporterConfig = { width :: Int }

dotReporter :: DotReporterConfig -> Reporter
dotReporter { width } = defaultReporter (-1) $ logWriter <<< case _ of
  Event.TestEnd _ _ (Success speed _) -> wrap $ colored (Speed.toColor speed) "."
  Event.TestEnd _ _ (Failure _) -> wrap $ colored Color.Fail "!"
  Event.Pending _ _ -> wrap $ colored Color.Pass ","
  Event.End _ -> tellLn ""
  _ -> pure unit
  where
    wrap action = do
      n <- modify (_ + 1)
      when (n `mod` width == 0) do
        tellLn ""
      tell action
