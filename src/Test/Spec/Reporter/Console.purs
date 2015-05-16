module Test.Spec.Reporter.Console (
  consoleReporter
  )
  where

import Debug.Trace
import Data.String
import Data.Array (map, concatMap)
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Extras
import qualified Test.Spec as S
import Test.Spec.Errors
import Test.Spec.Console
import Test.Spec.Summary
import Test.Spec.Reporter

pluralize :: String -> Number -> String
pluralize s 1 = s
pluralize s _ = s ++ "s"

printPassedFailed :: forall r. Number -> Number -> Eff (trace :: Trace | r) Unit
printPassedFailed p f = do
  let total = p + f
      testStr = pluralize "test" total
      amount = show p ++ "/" ++ (show total) ++ " " ++ testStr ++ " passed"
      attrs = if f > 0 then [31] else [32]
  withAttrs attrs $ writeln amount

printPending :: forall r. Number -> Eff (trace :: Trace | r) Unit
printPending p =
  if p > 0 then withAttrs [33] do write $ show p
                                  write " "
                                  write (pluralize "test" p)
                                  writeln " pending"
           else return unit

printSummary' :: forall r. Summary -> Eff (trace :: Trace | r) Unit
printSummary' (Count passed failed pending) = do
  writeln ""
  withAttrs [1] $ writeln "Summary"
  printPassedFailed passed failed
  printPending pending
  writeln ""

printSummary :: forall r. [S.Group] -> Eff (trace :: Trace | r) Unit
printSummary groups = printSummary' $ summarize groups

printEntry :: forall r. Entry
           -> Eff (trace :: Trace | r) Unit
printEntry (It name S.Success) = do
  withAttrs [32] $ writeln $  "✓︎ " ++ name
printEntry (Pending name) = do
  withAttrs [33] $ writeln $  "~ " ++ name
printEntry (It name (S.Failure err)) = do
  withAttrs [31] $ writeln $ "✗ " ++ name ++ ":"
  trace ""
  withAttrs [31] $ writeln $ "  " ++ errorMessage err
printEntry (Describe n) = do
  writeln ""
  printNames n
  where printNames [] = return unit
        printNames [last] = withAttrs [1, 35] $ writeln last
        printNames (name : names) = do
          withAttrs [1] do
            write name
            write " » "
          printNames names

consoleReporter :: forall e. Reporter (trace :: Trace | e)
consoleReporter groups = do
  mapM_ printEntry $ concatMap collapse groups
  printSummary groups
