module Test.Spec.Reporter.Summary where

import Debug.Trace
import Control.Monad.Eff
import Test.Spec
import Test.Spec.Summary
import Test.Spec.Console

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

printSummary :: forall r. [Group] -> Eff (trace :: Trace | r) Unit
printSummary groups = printSummary' $ summarize groups
