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
import Control.Monad.Eff.Random
import Test.Spec

type Runner r t = StateT [Group] (Eff r) t

foreign import setAttr
  """
  function setAttr(code) {
    return function () {
      if (process) {
        process.stdout.write("\x1b[" + code + "m");
      }
    };
  };
  """ :: forall e. Number -> Eff (trace :: Trace | e) Unit

reset :: forall e. Eff (trace :: Trace | e) Unit
reset = setAttr 0

withAttrs :: forall r. [Number] -> Eff (trace :: Trace | r) Unit -> Eff (trace :: Trace | r) Unit
withAttrs [] r = r
withAttrs (attr : attrs) r = do
  setAttr attr
  withAttrs attrs r
  reset

describe :: forall r. String
         -> Runner r Unit
         -> Runner r Unit
describe name its = do
  its
  results <- get
  put $ [Describe name results]
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

mapM_ :: forall m a b. (Monad m) => (a -> m b) -> [a] -> m Unit
mapM_ _ [] = return unit
mapM_ f (x : xs) = do
  f x
  mapM_ f xs

foreign import showAssertionError
  """
  function showAssertionError(err) {
    return err.message;
  }
  """ :: Error -> String

printGroup :: forall r. Group
            -> Eff (trace :: Trace | r) Unit
printGroup (It name Success ) = do
  withAttrs [32] $ trace $  "√ " ++ name
printGroup (Pending name) = do
  withAttrs [33] $ trace $  "~ " ++ name ++ " (pending)"
printGroup (It name (Failure err)) = do
  withAttrs [31] $ trace $ "✗ " ++ name ++ ":"
  trace ""
  withAttrs [31] $ trace $ "  " ++ showAssertionError err
  trace ""
printGroup (Describe name groups) = do
  trace ""
  withAttrs [1, 4] $ trace $ name
  mapM_ printGroup groups
  trace ""

suite :: forall r. Runner (trace :: Trace | r) Unit
      -> Eff (trace :: Trace | r) Unit
suite r = do
  pair <- runStateT r []
  let groups = snd pair
  -- TODO: Separate console printing as a pluggable "Reporter"
  mapM_ printGroup groups
  return unit

