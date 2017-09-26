module Test.Spec.Color where

import Ansi.Codes (GraphicsParam(..), colorSuffix, graphicsParamToString, prefix)
import Data.Semigroup ((<>))
import Data.Show (show)
import Prelude ((<<<))

data Color
 = Pass
 | Fail
 | Pending
 | Suite
 | ErrorTitle
 | ErrorMessage
 | ErrorStack
 | Checkmark
 | Fast
 | Medium
 | Slow
 | Green
 | Light

code :: Color -> Int
code Pass         = 2
code Fail         = 31
code Pending      = 36
code Suite        = 0
code ErrorTitle   = 0
code ErrorMessage = 31
code ErrorStack   = 2
code Checkmark    = 32
code Fast         = 2
code Medium       = 33
code Slow         = 31
code Green        = 32
code Light        = 2

colored :: Color -> String -> String
colored = _colored <<< code

_colored :: Int -> String -> String
_colored c str = wrap (show c) <> str <> wrap (graphicsParamToString Reset)
  where
  wrap s = prefix <> s <> colorSuffix
