module Test.Spec.Console
  ( setAttr
  , reset
  , withAttrs
  , write
  , logWriter
  , moveUpAndClearLine
  ) where

import Prelude

import Ansi.Codes (colorSuffix, prefix)
import Control.Monad.Writer (class MonadWriter, Writer, execWriter, tell)
import Data.Foldable (foldr)
import Effect (Effect)

foreign import write :: String -> Effect Unit
foreign import moveUpAndClearLine :: Effect Unit

logWriter :: Writer String Unit -> Effect Unit
logWriter = execWriter >>> write

setAttr
  :: forall m
   . MonadWriter String m
  => Int
  -> m Unit
setAttr code = tell $ prefix <> show code <> colorSuffix

reset
  :: forall m
   . MonadWriter String m
  => m Unit
reset = setAttr 0

withAttrs
  :: forall m
   . MonadWriter String m
  => Array Int
  -> m Unit
  -> m Unit
withAttrs as r = foldr iter r as
  where iter attr acc = setAttr attr *> acc *> reset
