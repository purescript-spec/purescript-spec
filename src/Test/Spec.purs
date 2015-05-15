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
import Data.Either
import Data.String (joinWith, split)
import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Extras
import Control.Monad.Aff

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

type Spec r t = StateT [Group] (Aff r) t

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

runCatch :: forall r. String
         -> Aff r Unit
         -> Aff r Group
runCatch name tests = do
  e <- attempt tests
  either onError onSuccess e
  where
  onError e = return $ It name $ Failure e
  onSuccess _ = return $ It name Success

it :: forall r. String
    -> Aff r Unit
    -> Spec r Unit
it description tests =
  do
    result <- lift $ runCatch description tests
    modify $ \p -> p ++ [result]
    return unit

collect :: forall r. Spec r Unit
        -> Aff r [Group]
collect r = do
  c <- runStateT r []
  return $ snd c
