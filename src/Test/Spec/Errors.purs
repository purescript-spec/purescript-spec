module Test.Spec.Errors where

import Control.Monad.Eff.Exception

foreign import errorMessage
  """
  function errorMessage(err) {
    return err.message;
  }
  """ :: Error -> String

foreign import errorName
  """
  function errorName(err) {
    return err.name;
  }
  """ :: Error -> String

foreign import errorStackTrace
  """
  function errorStackTrace(err) {
    return err.stack;
  }
  """ :: Error -> String
