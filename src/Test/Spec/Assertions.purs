module Test.Spec.Assertions where

import Prelude

import Control.Monad               (when)
import Control.Monad.Aff           (Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class   (throwError)
import Data.Foldable               (Foldable, notElem)

fail :: forall r. String -> Aff r Unit
fail msg = throwError $ error $ msg

shouldEqual :: forall r t. (Show t, Eq t) => t -> t -> Aff r Unit
shouldEqual v1 v2 =
  when (v1 /= v2) $
    fail $ show v1 ++ " ≠ " ++ show v2

shouldNotEqual :: forall r t. (Show t, Eq t) => t -> t -> Aff r Unit
shouldNotEqual v1 v2 =
  when (v1 == v2) $
    fail $ show v1 ++ " = " ++ show v2

shouldContain :: forall r f a. (Show a, Eq a, Show (f a), Foldable f) => f a -> a -> Aff r Unit
shouldContain c e =
  when (e `notElem` c) $
    fail $ (show e) ++ " ∉ " ++ (show c)
