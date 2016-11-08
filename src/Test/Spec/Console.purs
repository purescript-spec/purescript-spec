module Test.Spec.Console
  ( setAttr
  , reset
  , withAttrs
  , write
  ) where

import Prelude

import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foldable             (foldr)

-- This needs a foreign function to support the escape sequence.
foreign import _setAttr :: forall e. String -> Eff (console :: CONSOLE | e) Unit
foreign import write :: forall e. String -> Eff (console :: CONSOLE | e) Unit

setAttr :: forall e. Int -> Eff (console :: CONSOLE | e) Unit
setAttr code = _setAttr (show code)

reset :: forall e. Eff (console :: CONSOLE | e) Unit
reset = setAttr 0

withAttrs :: forall r. (Array Int)
          -> Eff (console :: CONSOLE | r) Unit
          -> Eff (console :: CONSOLE | r) Unit
withAttrs as r = foldr iter r as
  where iter attr acc = setAttr attr *> acc *> reset
