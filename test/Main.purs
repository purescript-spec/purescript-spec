module Test.Main where

import Prelude

import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Node.Process              (PROCESS())

import Test.Spec.Runner         (run)
import Test.Spec.Reporter       (dotReporter)

import Test.Spec.ReporterSpec   (reporterSpec)
import Test.Spec.RunnerSpec     (runnerSpec)
import Test.Spec.AssertionSpec  (assertionSpec)

main :: forall eff. Eff (console :: CONSOLE, process :: PROCESS | eff) Unit
main = run [dotReporter] do
  runnerSpec
  reporterSpec
  assertionSpec
