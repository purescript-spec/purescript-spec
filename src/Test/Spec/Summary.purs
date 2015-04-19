module Test.Spec.Summary where

import Test.Spec

data Summary = Count Number Number Number

instance semigroupCount :: Semigroup Summary where
  (<>) (Count p1 f1 s1) (Count p2 f2 s2) = Count (p1 + p2) (f1 + f2) (s1 + s2)

summarize :: [Group] -> Summary
summarize [] = Count 0 0 0
summarize ((It _ Success):gs) = (Count 1 0 0) <> summarize gs
summarize ((It _ (Failure _)):gs) = (Count 0 1 0) <> summarize gs
summarize ((Pending _):gs) = (Count 0 0 1) <> summarize gs
summarize ((Describe _ dgs):gs) = summarize dgs <> summarize gs

successful :: [Group] -> Boolean
successful groups =
  case summarize groups of
    (Count _ 0 _) ->  true
    _ -> false
