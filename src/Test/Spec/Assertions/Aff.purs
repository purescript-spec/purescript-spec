module Test.Spec.Assertions.Aff (
  expectError
  ) where

import Prelude

import Control.Monad.Aff           (Aff, attempt)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class   (throwError)
import Data.Either                 (Either(..))

expectError :: forall r t. Aff r t -> Aff r Unit
expectError a = do
  e <- attempt a
  case e of
    Left _ -> pure unit
    Right _ -> throwError $ error "Expected error"
