module Test.Spec.Assertions
  ( fail
  , shouldEqual
  , shouldNotEqual
  , shouldContain
  , shouldNotContain
  , shouldNotSatisfy
  , shouldSatisfy
  ) where

import Prelude

import Effect.Aff                  (Aff())
import Effect.Exception            (error)
import Control.Monad.Error.Class   (throwError)
import Data.Foldable               (class Foldable, notElem, elem)

fail :: String -> Aff Unit
fail msg = throwError $ error $ msg

shouldEqual :: forall t. Show t => Eq t => t -> t -> Aff Unit
shouldEqual v1 v2 =
  when (v1 /= v2) $
    fail $ show v1 <> " ≠ " <> show v2

shouldNotEqual :: forall t. Show t => Eq t => t -> t -> Aff Unit
shouldNotEqual v1 v2 =
  when (v1 == v2) $
    fail $ show v1 <> " = " <> show v2

shouldSatisfy :: forall t. Show t => t -> (t -> Boolean) -> Aff Unit
shouldSatisfy v pred =
  unless (pred v) $
    fail $ show v <> " doesn't satisfy predicate"

shouldNotSatisfy :: forall t. Show t => t -> (t -> Boolean) -> Aff Unit
shouldNotSatisfy v pred =
  when (pred v) $
    fail $ show v <> " satisfies predicate, but should not"

shouldContain :: forall f a. Show a => Eq a => Show (f a) => Foldable f => f a -> a -> Aff Unit
shouldContain c e =
  when (e `notElem` c) $
    fail $ (show e) <> " ∉ " <> (show c)

shouldNotContain :: forall f a. Show a => Eq a => Show (f a) => Foldable f => f a -> a -> Aff Unit
shouldNotContain c e =
  when (e `elem` c) $
    fail $ (show e) <> " ∈ " <> (show c)
