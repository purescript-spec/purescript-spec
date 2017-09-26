module Test.Spec.Console
  ( setAttr
  , reset
  , withAttrs
  , write
  ) where

import Prelude

import Ansi.Codes (colorSuffix, prefix)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foldable (foldr)

foreign import write :: forall e. String -> Eff (console :: CONSOLE | e) Unit

setAttr :: forall e. Int -> Eff (console :: CONSOLE | e) Unit
setAttr code = write (prefix <> show code <> colorSuffix)

reset :: forall e. Eff (console :: CONSOLE | e) Unit
reset = setAttr 0

withAttrs :: forall r. (Array Int)
          -> Eff (console :: CONSOLE | r) Unit
          -> Eff (console :: CONSOLE | r) Unit
withAttrs as r = foldr iter r as
  where iter attr acc = setAttr attr *> acc *> reset
