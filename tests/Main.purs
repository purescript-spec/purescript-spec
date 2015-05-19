module Main where

import Test.Spec.Node
import Test.Spec.Reporter.Console
import Test.Spec.RunnerSpec
import Test.Spec.ReporterSpec

main = runNode [consoleReporter] do
  runnerSpec
  reporterSpec
