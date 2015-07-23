module Test.Spec.Assertions.String (
  shouldContain,
  shouldNotContain
  ) where

import Prelude

import Control.Monad               (when)
import Control.Monad.Aff           (Aff())
import Control.Monad.Error.Class   (throwError)
import Control.Monad.Eff.Exception (error)
import Data.String                 (contains)

shouldContain :: forall r. String -> String -> Aff r Unit
shouldContain s subs =
  when (not $ contains subs s) $
    throwError $ error $ show subs ++ " ∉ " ++ show s

shouldNotContain :: forall r. String -> String -> Aff r Unit
shouldNotContain s subs =
  when (contains subs s) $
    throwError $ error $ show subs ++ " ∈ " ++ show s
