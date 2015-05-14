module Test.Spec.Assertions where

import Control.Monad.Aff
import Control.Monad.Eff.Exception

shouldEqual :: forall r t. (Show t, Eq t) => t -> t -> Aff r Unit
shouldEqual v1 v2 =
  makeAff f
  where f onError onSuccess = if v1 == v2
                                then onSuccess unit
                                else onError $ error msg
                                    where s1 = show v1
                                          s2 = show v2
                                          msg = s1 ++ " ≠ " ++ s2

shouldNotEqual :: forall r t. (Show t, Eq t) => t -> t -> Aff r Unit
shouldNotEqual v1 v2 =
  makeAff f
  where f onError onSuccess = if v1 /= v2
                                then onSuccess unit
                                else onError $ error msg
                                    where s1 = show v1
                                          s2 = show v2
                                          msg = s1 ++ " = " ++ s2
