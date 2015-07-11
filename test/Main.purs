module Test.Main where

import Prelude

import Test.Spec.Node             (runNode)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Spec.ReporterSpec (reporterSpec)
import Test.Spec.RunnerSpec   (runnerSpec)

main = runNode [consoleReporter] do
  runnerSpec
  reporterSpec
