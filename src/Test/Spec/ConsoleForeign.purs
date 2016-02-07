module Test.Spec.ConsoleForeign
  ( write
  , _setAttr
  , supportedEnvironment
  , consoleLog) where

import Prelude                     (Unit)

import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Console   (CONSOLE)

foreign import write :: forall e. String -> Eff (console :: CONSOLE | e) Unit

-- This needs a foreign function to support the escape sequence.
foreign import _setAttr :: forall e. String -> Eff (console :: CONSOLE | e) Unit

foreign import supportedEnvironment :: Boolean

foreign import consoleLog :: forall e. String -> Eff (console :: CONSOLE | e) Unit