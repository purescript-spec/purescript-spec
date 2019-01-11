module Test.Spec.Reporter.Dot (dotReporter) where

import Prelude
import Test.Spec.Color as Color
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed
import Test.Spec.Color (colored)
import Test.Spec.Console (write) as Console
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Runner (Reporter)

type DotReporterState = Int
type DotReporterConfig = { width :: Int }

dotReporter :: DotReporterConfig -> Reporter
dotReporter { width } =
  defaultReporter (-1) update

  where
    update n = case _ of
      Event.Pass _ _ speed ms ->
        let col = Speed.toColor speed
        in wrap $ Console.write (colored col ".")
      Event.Fail _ _ _ _ -> wrap $ Console.write (colored Color.Fail "!")
      Event.Pending _ _ -> wrap $ Console.write (colored Color.Pass ",")
      Event.End _ -> n <$ Console.write "\n"
      _ -> pure n

      where
        wrap action =
          let n' = n + 1
          in n' <$ do
            when (n' `mod` width == 0) (Console.write "\n")
            action
