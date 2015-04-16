module Test.Spec.Summary where

import Debug.Trace
import Control.Monad.Eff
import Test.Spec
import Test.Spec.Console

data Summary = Count Number Number Number

instance semigroupCount :: Semigroup Summary where
  (<>) (Count p1 f1 s1) (Count p2 f2 s2) = Count (p1 + p2) (f1 + f2) (s1 + s2)

summarize :: [Group] -> Summary
summarize [] = Count 0 0 0
summarize ((It _ Success):gs) = (Count 1 0 0) <> summarize gs
summarize ((It _ (Failure _)):gs) = (Count 0 1 0) <> summarize gs
summarize ((Pending _):gs) = (Count 0 0 1) <> summarize gs
summarize ((Describe _ dgs):gs) = summarize dgs <> summarize gs

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

printSkipped :: forall r. Number -> Eff (trace :: Trace | r) Unit
printSkipped s =
  if s > 0 then withAttrs [33] do write $ show s
                                  write " "
                                  write (pluralize "test" s)
                                  writeln " skipped"
           else return unit

printSummary' :: forall r. Summary -> Eff (trace :: Trace | r) Unit
printSummary' (Count p f s) = withAttrs [1] do
  withAttrs [4] $ writeln "Summary"
  printPassedFailed p f
  printSkipped s
  writeln ""

printSummary :: forall r. [Group] -> Eff (trace :: Trace | r) Unit
printSummary groups = printSummary' $ summarize groups
