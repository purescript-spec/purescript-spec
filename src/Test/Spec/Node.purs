module Test.Spec.Node (
  Process(..),
  runNode
  ) where

import Prelude

import Control.Monad             (when)
import Control.Monad.Aff         (runAff)
import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE(), print)
import Data.Foldable             (sequence_)

import Test.Spec          (Spec(), collect)
import Test.Spec.Console  (withAttrs)
import Test.Spec.Summary  (successful)
import Test.Spec.Reporter (Reporter())

foreign import data Process :: !

foreign import exit :: forall eff. Int -> Eff (process :: Process | eff) Unit

runNode :: forall e r.
        Array (Reporter (process :: Process, console :: CONSOLE | e))
        -> Spec (process :: Process, console :: CONSOLE | e) Unit
        -> Eff  (process :: Process, console :: CONSOLE | e) Unit
runNode rs spec = do
  runAff
    (\err -> do withAttrs [31] $ print err
                exit 1)
    (\results -> do sequence_ (map (\f -> f results) rs)
                    when (not $ successful results) $ exit 1)
    (collect spec)
