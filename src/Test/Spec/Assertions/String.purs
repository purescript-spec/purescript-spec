module Test.Spec.Assertions.String
  ( shouldContain
  , shouldNotContain
  , shouldStartWith
  , shouldEndWith
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff)
import Effect.Exception (error)

foreign import _startsWith :: String -> String -> Boolean
foreign import _endsWith :: String -> String -> Boolean

-- | Asserts `string` starts with `prefix`
-- |
-- | ```purescript
-- | string `shouldStartWith` prefix
-- | ```
shouldStartWith :: String -> String -> Aff Unit
shouldStartWith s prefix =
  when (not $ _startsWith prefix s) $
     throwError $ error $ show s <> " does not start with " <> show prefix

-- | Asserts `string` ends with `suffix`
-- |
-- | ```purescript
-- | string `shouldEndWith` suffix
-- | ```
shouldEndWith :: String -> String -> Aff Unit
shouldEndWith s suffix =
  when (not $ _endsWith suffix s) $
     throwError $ error $ show s <> " does not end with " <> show suffix

-- | Asserts `string` contains `subs`
-- |
-- | ```purescript
-- | string `shouldContain` subs
-- | ```
shouldContain :: String -> String -> Aff Unit
shouldContain s subs =
  when (not $ contains (Pattern subs) s) $
    throwError $ error $ show subs <> " ∉ " <> show s

-- | Asserts `string` does not contain `subs`
-- |
-- | ```purescript
-- | string `shouldContain` subs
-- | ```
shouldNotContain :: String -> String -> Aff Unit
shouldNotContain s subs =
  when (contains (Pattern subs) s) $
    throwError $ error $ show subs <> " ∈ " <> show s
