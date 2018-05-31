module Test.Spec.Assertions
  ( fail
  , shouldEqual
  , shouldNotEqual
  , shouldContain
  , shouldNotContain
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

shouldContain :: forall f a. Show a => Eq a => Show (f a) => Foldable f => f a -> a -> Aff Unit
shouldContain c e =
  when (e `notElem` c) $
    fail $ (show e) <> " ∉ " <> (show c)

shouldNotContain :: forall f a. Show a => Eq a => Show (f a) => Foldable f => f a -> a -> Aff Unit
shouldNotContain c e =
  when (e `elem` c) $
    fail $ (show e) <> " ∈ " <> (show c)
