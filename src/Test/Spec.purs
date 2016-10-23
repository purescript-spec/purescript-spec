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

import Control.Monad.Aff           (Aff())
import Control.Monad.Eff.Exception (Error())
import Control.Monad.State         (State(), modify, runState)
import Data.Tuple                  (snd)

type Name = String

data Group t
  = Describe Name (Array (Group t))
  | It Name t
  | Pending Name

data Result
  = Success
  | Failure Error

instance showResult :: Show Result where
  show Success = "Success"
  show (Failure err) = "Failure (Error ...)"

instance eqResult :: Eq Result where
  eq Success Success = true
  eq (Failure _) (Failure _) = true
  eq _ _ = false

instance showGroup :: Show t => Show (Group t) where
  show (Describe name groups) = "Describe " <> show name <> " " <> show groups
  show (It name test) = "It " <> show name <> " " <> show test
  show (Pending name) = "Describe " <> show name

instance eqGroup :: Eq t => Eq (Group t) where
  eq (Describe n1 g1) (Describe n2 g2) = n1 == n2 && g1 == g2
  eq (It n1 t1)       (It n2 t2)       = n1 == n2 && t1 == t2
  eq (Pending n1)     (Pending n2)     = n1 == n2
  eq _                _                = false

-- Specifications with unevaluated tests.
type Spec r t = State (Array (Group (Aff r Unit))) t

collect :: forall r. Spec r Unit
        -> Array (Group (Aff r Unit))
collect r = snd $ runState r []

---------------------
--       DSL       --
---------------------

describe :: forall r. String
         -> Spec r Unit
         -> Spec r Unit
describe name its = do
  modify $ \r -> r <> [Describe name (collect its)]
  pure unit

pending :: forall r. String
        -> Spec r Unit
pending name = modify $ \p -> p <> [Pending name]

it :: forall r. String
   -> Aff r Unit
   -> Spec r Unit
it description tests =
  do
    modify $ \p -> p <> [It description tests]
    pure unit
