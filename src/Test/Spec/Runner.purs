module Test.Spec.Runner where

import Debug.Trace
import Data.Maybe
import Data.Tuple
import Data.Array
import Data.String (joinWith, split)
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Extras
import Test.Spec
import Test.Spec.Console
import Test.Spec.Reporter (report)

foreign import data Process :: !

foreign import exit
  """
  function exit(code) {
    return function() {
      process.exit(code);
    };
  }
  """ :: forall eff. Number -> Eff (process :: Process | eff) Unit

type Runner r t = StateT [Group] (Eff r) t

describe :: forall r. String
         -> Runner (trace :: Trace | r) Unit
         -> Runner (trace :: Trace | r) Unit
describe name its = do
  results <- lift $ collect its
  modify $ \r -> r ++ [Describe name results]
  return unit

run :: forall r. String
    -> Eff (err :: Exception | r) Unit
    -> Runner r Group
run name tests =
  lift $ catchException printErr onSuccess
  where onSuccess = do tests
                       return $ It name $ Success
        printErr err = return $ It name $ Failure err

pending :: forall r. String
        -> Runner r Unit
pending name = modify $ \p -> p ++ [Pending name]


it :: forall r. String
   -> Eff (err :: Exception | r) Unit
   -> Runner r Unit
it description tests = do
  result <- run description tests
  modify $ \p -> p ++ [result]
  return unit

collect :: forall r. Runner (trace :: Trace | r) Unit
        -> Eff (trace :: Trace | r) [Group]
collect r = do
  pair <- runStateT r []
  return $ snd pair

suite :: forall r. Runner (trace :: Trace | r) Unit
      -> Eff (trace :: Trace | r) Unit
suite r = do
  results <- collect r
  -- TODO: Separate console printing as a pluggable "Reporter"
  report $ results
  return unit

