module Test.Spec.Speed where

import Prelude
import Data.Generic (class Generic, gShow)
import Test.Spec.Color as Color
import Test.Spec.Color (Color)

data Speed = Fast | Medium | Slow

derive instance genericSpeed :: Generic Speed

instance showSpeed :: Show Speed
  where show = gShow

speedOf :: Int -> Int -> Speed
speedOf thresh ms | ms > thresh     = Slow
speedOf thresh ms | ms > thresh / 2 = Medium
speedOf _      _                    = Fast

toColor' :: Int -> Int -> Color
toColor' thresh ms = toColor $ speedOf thresh ms

toColor :: Speed -> Color
toColor Fast   = Color.Fast
toColor Medium = Color.Medium
toColor Slow   = Color.Slow

