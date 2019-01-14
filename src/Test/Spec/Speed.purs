module Test.Spec.Speed where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Test.Spec.Color (Color)
import Test.Spec.Color as Color

data Speed = Fast | Medium | Slow

derive instance genericSpeed :: Generic Speed _
instance showSpeed :: Show Speed where show = genericShow
instance showEq :: Eq Speed where eq = genericEq

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

