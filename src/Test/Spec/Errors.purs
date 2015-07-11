module Test.Spec.Errors where

import Control.Monad.Eff.Exception (Error())

foreign import errorMessage :: Error -> String

foreign import errorName :: Error -> String

foreign import errorStackTrace :: Error -> String
