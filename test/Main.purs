module Test.Main where

import Prelude

import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Spec.ReporterSpec   (reporterSpec)
import Test.Spec.RunnerSpec     (runnerSpec)
import Test.Spec.AssertionSpec  (assertionSpec)

main = run [consoleReporter] do
  runnerSpec
  reporterSpec
  assertionSpec
