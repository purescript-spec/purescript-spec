module Test.Spec.Assertions.Aff
  ( expectError
  , shouldReturn
  , shouldNotReturn
  ) where

import Prelude

import Data.Either                 (Either(..))
import Effect.Aff                  (Aff, attempt)
import Effect.Exception            (error)
import Control.Monad.Error.Class   (throwError)
import Test.Spec.Assertions        (shouldEqual, shouldNotEqual)

-- | Assert `Aff t` throws an error
expectError :: forall t. Aff t -> Aff Unit
expectError a = do
  e <- attempt a
  case e of
    Left _ -> pure unit
    Right _ -> throwError $ error "Expected error"

-- | Asserts that `Aff t` returns `t`
shouldReturn :: forall t. Eq t => Show t => Aff t -> t -> Aff Unit
shouldReturn ft t = ft >>= (_ `shouldEqual` t)

-- | Asserts that `Aff t` does not return `t`
shouldNotReturn :: forall t. Eq t => Show t => Aff t -> t -> Aff Unit
shouldNotReturn ft t = ft >>= (_ `shouldNotEqual` t)
