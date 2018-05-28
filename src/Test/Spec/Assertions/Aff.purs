module Test.Spec.Assertions.Aff (
  expectError
  ) where

import Prelude

import Effect.Aff                  (Aff, attempt)
import Effect.Exception            (error)
import Control.Monad.Error.Class   (throwError)
import Data.Either                 (Either(..))

expectError :: forall t. Aff t -> Aff Unit
expectError a = do
  e <- attempt a
  case e of
    Left _ -> pure unit
    Right _ -> throwError $ error "Expected error"
