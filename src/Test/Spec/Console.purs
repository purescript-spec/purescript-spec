module Test.Spec.Console where

import Prelude

import Control.Apply             ((*>))
import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Data.Foldable             (foldr)

foreign import write :: forall e. String -> Eff (console :: CONSOLE | e) Unit

writeln :: forall e. String -> Eff (console :: CONSOLE | e) Unit
writeln s = write $ s <> "\n"

setAttr :: forall e. Int -> Eff (console :: CONSOLE | e) Unit
setAttr code = write $ "\x1b[" <> show code <> "m"

reset :: forall e. Eff (console :: CONSOLE | e) Unit
reset = setAttr 0

withAttrs :: forall r. (Array Int)
          -> Eff (console :: CONSOLE | r) Unit
          -> Eff (console :: CONSOLE | r) Unit
withAttrs as r = foldr iter r as
  where iter attr acc = setAttr attr *> acc *> reset
