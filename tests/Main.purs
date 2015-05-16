module Main where

import Test.Spec.Node
import Test.Spec.Reporter.Console
import Test.Spec.Reporter.Xunit
import Test.Spec.RunnerSpec
import Test.Spec.ReporterSpec
import Test.Spec.Reporter.XunitSpec
import Data.XML.PrettyPrintSpec

main = runNode [consoleReporter, xunitReporter "output/test.xml"] do
  runnerSpec
  reporterSpec
  xunitSpec
  prettyPrintSpec
