module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Process (PROCESS)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.Reporter (dotReporter, specReporter)
import Test.Spec.ReporterSpec (reporterSpec)
import Test.Spec.Runner (run)
import Test.Spec.RunnerSpec (runnerSpec)

main :: forall eff. Eff (console :: CONSOLE, process :: PROCESS | eff) Unit
main = run [specReporter] do
  runnerSpec
  reporterSpec
  assertionSpec
