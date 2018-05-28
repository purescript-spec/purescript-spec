module Test.Spec.Assertions.String (
  shouldContain,
  shouldNotContain
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff)
import Effect.Exception (error)

shouldContain :: String -> String -> Aff Unit
shouldContain s subs =
  when (not $ contains (Pattern subs) s) $
    throwError $ error $ show subs <> " ∉ " <> show s

shouldNotContain :: String -> String -> Aff Unit
shouldNotContain s subs =
  when (contains (Pattern subs) s) $
    throwError $ error $ show subs <> " ∈ " <> show s
