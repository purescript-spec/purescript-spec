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

import Prelude

import Control.Monad.Aff           (Aff(), attempt, later)
import Control.Monad.Eff.Exception (Error())
import Control.Monad.State.Class   (modify)
import Control.Monad.State.Trans   (StateT(), runStateT)
import Control.Monad.Trans         (lift)
import Data.Either                 (either)
import Data.Tuple                  (snd)

type Name = String

data Result = Success
            | Failure Error

data Group = Describe Name (Array Group)
           | It Name Result
           | Pending Name

instance showResult :: Show Result where
  show Success = "Success"
  show (Failure err) = "Failure (Error ...)"

instance eqResult :: Eq Result where
  eq Success Success = true
  eq (Failure _) (Failure _) = true
  eq _ _ = false

instance showGroup :: Show Group where
  show (Describe name groups) = "Describe " <> show name <> " " <> show groups
  show (It name result) = "It " <> show name <> " " <> show result
  show (Pending name) = "Describe " <> show name

instance eqGroup :: Eq Group where
  eq (Describe n1 g1) (Describe n2 g2) = n1 == n2 && g1 == g2
  eq (It n1 r1)       (It n2 r2)       = n1 == n2 && r1 == r2
  eq (Pending n1)     (Pending n2)     = n1 == n2
  eq _                _                = false

type Spec r t = StateT (Array Group) (Aff r) t

describe :: forall r. String
         -> Spec r Unit
         -> Spec r Unit
describe name its = do
  results <- lift $ collect its
  modify $ \r -> r <> [Describe name results]
  pure unit

pending :: forall r. String
        -> Spec r Unit
pending name = modify $ \p -> p <> [Pending name]

runCatch :: forall r. String
         -> Aff r Unit
         -> Aff r Group
runCatch name tests = do
  e <- attempt tests
  either onError onSuccess e
  where
  onError e = pure $ It name $ Failure e
  onSuccess _ = pure $ It name Success

it :: forall r. String
    -> Aff r Unit
    -> Spec r Unit
it description tests =
  do
    result <- lift $ later $ runCatch description tests
    modify $ \p -> p <> [result]
    pure unit

collect :: forall r. Spec r Unit
        -> Aff r (Array Group)
collect r = do
  c <- runStateT r []
  pure $ snd c
