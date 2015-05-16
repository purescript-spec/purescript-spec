module Test.Spec.Assertions.String where

import Data.Maybe
import Data.String
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class

shouldContain :: forall r. String -> String -> Aff r Unit
shouldContain s subs =
  when (indexOf subs s < 0) $
    throwError $ error $ show subs ++ " ∉ " ++ show s
