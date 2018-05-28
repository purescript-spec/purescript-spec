module Test.Spec.Console
  ( setAttr
  , reset
  , withAttrs
  , write
  ) where

import Prelude

import Ansi.Codes (colorSuffix, prefix)
import Effect (Effect)
import Data.Foldable (foldr)

foreign import write :: String -> Effect Unit

setAttr :: Int -> Effect Unit
setAttr code = write (prefix <> show code <> colorSuffix)

reset :: Effect Unit
reset = setAttr 0

withAttrs :: (Array Int)
          -> Effect Unit
          -> Effect Unit
withAttrs as r = foldr iter r as
  where iter attr acc = setAttr attr *> acc *> reset
