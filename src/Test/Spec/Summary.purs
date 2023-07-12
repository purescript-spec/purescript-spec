module Test.Spec.Summary (
  Summary(..),
  summarize,
  successful
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Test.Spec.Result (Result(..))
import Test.Spec.Tree (Tree(..))

newtype Summary = Count { passed :: Int, failed :: Int, pending :: Int }
derive instance newtypeSummary :: Newtype Summary _

instance semigroupCount :: Semigroup Summary where
  append (Count c1) (Count c2) = Count $ c1 + c2

instance monoidCount :: Monoid Summary where
  mempty = Count zero

summarize :: ∀ n a. Array (Tree n a Result) -> Summary
summarize = foldMap case _ of
  (Leaf _ (Just (Success _ _))) -> Count { passed: 1, failed: 0, pending: 0 }
  (Leaf _ (Just (Failure _))) -> Count { passed: 0, failed: 1, pending: 0 }
  (Leaf _ Nothing) -> Count { passed: 0, failed: 0, pending: 1 }
  (Node _ dgs) -> summarize dgs

successful :: ∀ n a. Array (Tree n a Result) -> Boolean
successful groups = (un Count $ summarize groups).failed == 0
