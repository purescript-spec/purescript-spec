module Test.Spec where

import Control.Monad.Eff
import Control.Monad.Eff.Exception

type Name = String
data Result = Success
            | Failure Error
data Group = Describe Name [Group]
           | It Name Result
           | Pending Name

