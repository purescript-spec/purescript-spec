module Test.Main where

import Prelude

import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE())

import Test.Spec.Runner           (PROCESS(), run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Spec.ReporterSpec   (reporterSpec)
import Test.Spec.RunnerSpec     (runnerSpec)
import Test.Spec.AssertionSpec  (assertionSpec)

main :: forall eff. Eff (console :: CONSOLE, process :: PROCESS | eff) Unit
main = run [consoleReporter] do
  runnerSpec
  reporterSpec
  assertionSpec
