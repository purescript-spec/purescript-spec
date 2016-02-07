module Test.Spec.Reporter.Console (consoleReporter) where

import Prelude

import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Console   (CONSOLE(), log)
import Control.Monad.Eff.Exception (message)
import Data.Array                  (concatMap)
import Data.Foldable               (intercalate, traverse_)

import Test.Spec          (Group(), Result(..))
import Test.Spec.Console  (withAttrs, writeln)
import Test.Spec.ConsoleForeign  (write, supportedEnvironment, consoleLog)
import Test.Spec.Reporter (Entry(..), Reporter(), collapse)
import Test.Spec.Summary  (Summary(..), summarize)

pluralize :: String -> Int -> String
pluralize s 1 = s
pluralize s _ = s <> "s"

printPassedFailed :: forall r. Int -> Int -> Eff (console :: CONSOLE | r) Unit
printPassedFailed p f = do
  let total = p + f
      testStr = pluralize "test" total
      amount = show p ++ "/" ++ (show total) ++ " " ++ testStr ++ " passed"
      attrs = if f > 0 then [31] else [32]
  withAttrs attrs $ writeln amount

printPending :: forall r. Int -> Eff (console :: CONSOLE | r) Unit
printPending p =
  if p > 0 then withAttrs [33] do write $ show p
                                  write " "
                                  write (pluralize "test" p)
                                  writeln " pending"
           else return unit

printSummary' :: forall r. Summary -> Eff (console :: CONSOLE | r) Unit
printSummary' (Count passed failed pending) = do
  writeln ""
  withAttrs [1] $ writeln "Summary"
  printPassedFailed passed failed
  printPending pending
  writeln ""

printSummary :: forall r. Array Group -> Eff (console :: CONSOLE | r) Unit
printSummary groups = printSummary' $ summarize groups

printEntry :: forall r. Entry
           -> Eff (console :: CONSOLE | r) Unit
printEntry (It name Success) = do
  withAttrs [32] $ writeln $  "✓︎ " ++ name
printEntry (Pending name) = do
  withAttrs [33] $ writeln $  "~ " ++ name
printEntry (It name (Failure err)) = do
  withAttrs [31] $ writeln $ "✗ " ++ name ++ ":"
  log ""
  withAttrs [31] $ writeln $ "  " ++ message err
printEntry (Describe n) = do
  writeln ""
  printNames n
  where printNames ns = withAttrs [1, 35] $ writeln $ intercalate " » " ns

consoleReporter :: forall e. Reporter (console :: CONSOLE | e)
consoleReporter groups = do
  if not supportedEnvironment
    then
      consoleLog """Unsupported environment. The console reporter can only be run in a node-like
environment with access to process.stdout.write(). If you are running in the browser,
please see the mocha and xunit reporters. If using webpack, make sure you are targeting
node instead of the browser."""
    else do
      traverse_ printEntry $ concatMap collapse groups
      printSummary groups
