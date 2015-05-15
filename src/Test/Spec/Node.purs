module Test.Spec.Node (
  Process(..),
  runNode
  ) where

import Data.Foldable
import Data.Array
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Aff
import Control.Monad.Extras
import Test.Spec
import Test.Spec.Console
import Test.Spec.Summary
import qualified Test.Spec.Reporter as R

foreign import data Process :: !

foreign import exit
  """
  function exit(code) {
    return function() {
      process.exit(code);
    };
  }
  """ :: forall eff. Number -> Eff (process :: Process | eff) Unit

runNode :: forall e r.
        [([Group] -> Eff (process :: Process | e) Unit)]
        -> Spec (process :: Process | e) Unit
        -> Eff (process :: Process | e) Unit
runNode rs spec = do
  runAff
    (const $ exit 1)
    (\results -> do sequence_ (map (\f -> f results) rs)
                    when (not $ successful results) $ exit 1)
    (collect spec)
