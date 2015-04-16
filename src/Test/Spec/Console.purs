module Test.Spec.Console where

import Debug.Trace
import Control.Monad.Eff

foreign import write
  """
  function write(s) {
    return function () {
      if (process) {
        process.stdout.write(s);
      }
    };
  };
  """ :: forall e. String -> Eff (trace :: Trace | e) Unit

foreign import writeln
  """
  function writeln(s) {
    return function () {
      if (process) {
        process.stdout.write(s + "\n");
      }
    };
  };
  """ :: forall e. String -> Eff (trace :: Trace | e) Unit

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

