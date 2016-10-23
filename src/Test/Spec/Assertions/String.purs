module Test.Spec.Assertions.String (
  shouldContain,
  shouldNotContain
  ) where

import Prelude

import Control.Monad.Aff           (Aff())
import Control.Monad.Error.Class   (throwError)
import Control.Monad.Eff.Exception (error)
import Data.String                 (Pattern(..), contains)

shouldContain :: forall r. String -> String -> Aff r Unit
shouldContain s subs =
  when (not $ contains (Pattern subs) s) $
    throwError $ error $ show subs <> " ∉ " <> show s

shouldNotContain :: forall r. String -> String -> Aff r Unit
shouldNotContain s subs =
  when (contains (Pattern subs) s) $
    throwError $ error $ show subs <> " ∈ " <> show s
