module Test.Spec.Assertions where

import Control.Monad.Eff
import Control.Monad.Eff.Exception

shouldEqual :: forall r t. (Show t, Eq t) => t -> t -> Eff (err :: Exception | r) Unit
shouldEqual v1 v2 =
  if v1 == v2
    then return unit
    else throwException $ error msg
         where s1 = show v1
               s2 = show v2
               msg = s1 ++ " ≠ " ++ s2

shouldNotEqual :: forall r t. (Show t, Eq t) => t -> t -> Eff (err :: Exception | r) Unit
shouldNotEqual v1 v2 =
  if v1 /= v2
    then return unit
    else throwException $ error msg
         where s1 = show v1
               s2 = show v2
               msg = s1 ++ " = " ++ s2
