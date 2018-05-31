module Test.Spec.Summary (
  Summary(..),
  summarize,
  successful
  ) where

import Prelude

import Data.Foldable (foldMap)

import Test.Spec (Group(..), Result(..))

data Summary = Count Int Int Int

instance semigroupCount :: Semigroup Summary where
  append (Count p1 f1 s1) (Count p2 f2 s2) = Count (p1 + p2) (f1 + f2) (s1 + s2)

instance monoidCount :: Monoid Summary where
  mempty = Count 0 0 0

summarize :: Array (Group Result) -> Summary
summarize = foldMap \g -> case g of
    (It _ _ Success)     -> Count 1 0 0
    (It _ _ (Failure _)) -> Count 0 1 0
    (Pending _)          -> Count 0 0 1
    (Describe _ _ dgs)   -> summarize dgs

successful :: Array (Group Result) -> Boolean
successful groups =
  case summarize groups of
    (Count _ 0 _) -> true
    _             -> false
