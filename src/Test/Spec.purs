module Test.Spec (
  Name(..),
  Result(..),
  Group(..),
  Spec(..),
  describe,
  pending,
  it,
  collect
  ) where

import Debug.Trace
import Data.Maybe
import Data.Tuple
import Data.Array
import Data.String (joinWith, split)
import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Extras

type Name = String

data Result = Success
            | Failure Error

data Group = Describe Name [Group]
           | It Name Result
           | Pending Name

instance showResult :: Show Result where
  show Success = "Success"
  show (Failure err) = "Failure (Error ...)"

instance eqResult :: Eq Result where
  (==) Success Success = true
  (==) (Failure _) (Failure _) = true
  (==) _ _ = false
  (/=) r1 r2 = not (r1 == r2)

instance showGroup :: Show Group where
  show (Describe name groups) = "Describe " ++ show name ++ " " ++ show groups
  show (It name result) = "It " ++ show name ++ " " ++ show result
  show (Pending name) = "Describe " ++ show name

instance eqGroup :: Eq Group where
  (==) (Describe n1 g1) (Describe n2 g2) = n1 == n2 && g1 == g2
  (==) (It n1 r1) (It n2 r2) = n1 == n2 && r1 == r2
  (==) (Pending n1) (Pending n2) = n1 == n2
  (==) _ _ = false
  (/=) r1 r2 = not (r1 == r2)

type Spec r t = StateT [Group] (Eff r) t

describe :: forall r. String
         -> Spec r Unit
         -> Spec r Unit
describe name its = do
  results <- lift $ collect its
  modify $ \r -> r ++ [Describe name results]
  return unit

pending :: forall r. String
        -> Spec r Unit
pending name = modify $ \p -> p ++ [Pending name]


it :: forall r. String
   -> Eff (err :: Exception | r) Unit
   -> Spec r Unit
it description tests =
  do
    result <- run description tests
    modify $ \p -> p ++ [result]
    return unit
  where run name tests =
          lift $ catchException printErr onSuccess
          where onSuccess = do tests
                               return $ It name $ Success
                printErr err = return $ It name $ Failure err

collect :: forall r. Spec r Unit
        -> Eff r [Group]
collect r = do
  pair <- runStateT r []
  return $ snd pair
