module Test.Spec (
  Name(..),
  Only(..),
  Result(..),
  Execution(..),
  Group(..),
  Spec(..),
  Spec'(..),
  TestEnv,
  hoistSpec,
  hoistSpec',
  describe,
  describeOnly,
  parallel,
  sequential,
  pending,
  pending',
  it,
  itOnly,
  countTests
  ) where

import Prelude

import Control.Monad.State (execState)
import Control.Monad.State as State
import Control.Monad.Writer (Writer, execWriter, mapWriter, tell)
import Data.Array (snoc)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (Error)

type Name = String
type Only = Boolean

data Group t
  = Describe Only Name (Array (Group t))
  | SetExecution Execution (Array (Group t))
  | It Only Name t
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
  show (SetExecution execution groups) = "SetExecution " <> show execution <> " " <> show groups
  show (Describe only name groups) = "Describe " <> show only <> " " <> show name <> " " <> show groups
  show (It only name test) = "It " <> show only <> " " <> show name <> " " <> show test
  show (Pending name) = "Describe " <> show name

instance eqGroup :: Eq t => Eq (Group t) where
  eq (SetExecution e1 g1) (SetExecution e2 g2) = e1 == e2 && g1 == g2
  eq (Describe o1 n1 g1)  (Describe o2 n2 g2)  = o1 == o2 && n1 == n2 && g1 == g2
  eq (It o1 n1 t1)        (It o2 n2 t2)        = o1 == o2 && n1 == n2 && t1 == t2
  eq (Pending n1)         (Pending n2)         = n1 == n2
  eq _                    _                    = false

data Execution = Parallel | Sequential
instance showExecution :: Show Execution where
  show Parallel = "Parallel"
  show Sequential = "Sequential"

instance eqExecution :: Eq Execution where
  eq Parallel Parallel = true
  eq Sequential Sequential = true
  eq _ _ = false


-- Specifications with unevaluated tests.
type Spec t = Spec' Aff t
type Spec' m t = Writer (Array (Group (m Unit))) t

-- | Count the total number of tests in a spec
countTests :: forall m. Spec' m Unit -> Int
countTests spec = execState (for (execWriter spec) go) 0
  where
  go (SetExecution _ xs) = for_ xs go
  go (Describe _ _ xs) = for_ xs go
  go (It _ _ _) = State.modify_ (_ + 1)
  go (Pending _) = State.modify_ (_ + 1)


type TestEnv = { name :: NonEmptyArray Name, execution :: Execution }

hoistSpec :: forall m g. (m ~> g) -> Spec' m ~> Spec' g
hoistSpec f = hoistSpec' \_ -> f

hoistSpec' :: forall m g. (TestEnv -> m ~> g) -> Spec' m ~> Spec' g
hoistSpec' f = mapWriter \(Tuple a s) -> Tuple a $ map (go Sequential []) s
  where
    go :: Execution -> Array Name -> Group (m Unit) -> Group (g Unit)
    go execution ns = case _ of
      Describe o n rest -> Describe o n $ go execution (ns `snoc` n) <$> rest
      SetExecution execution' rest -> SetExecution execution' $ go execution' ns <$> rest
      It o n t -> It o n $ f { execution, name: ns `NEA.snoc'` n } t
      Pending n -> Pending n
---------------------
--       DSL       --
---------------------

-- | Combine a group of specs into a described hierarchy.
describe
  :: forall m
   . String
  -> Spec' m Unit
  -> Spec' m Unit
describe name its = tell [Describe false name (execWriter its)]

-- | Combine a group of specs into a described hierarchy and mark it as the
-- | only group to actually be evaluated. (useful for quickly narrowing down
-- | on a set)
describeOnly
  :: forall m
   . String
  -> Spec' m Unit
  -> Spec' m Unit
describeOnly name its = tell [Describe true name (execWriter its)]

-- | marks all spec items of the given spec to be safe for parallel evaluation.
parallel
  :: forall m
   . Spec' m Unit
  -> Spec' m Unit
parallel its = tell [SetExecution Parallel (execWriter its)]

-- | marks all spec items of the given spec to be evaluated sequentially.
sequential
  :: forall m
   . Spec' m Unit
  -> Spec' m Unit
sequential its = tell [SetExecution Sequential (execWriter its)]

-- | Create a pending spec.
pending
  :: forall m
   . String
  -> Spec' m Unit
pending name = tell [Pending name]

-- | Create a pending spec with a body that is ignored by
-- | the runner. It can be useful for documenting what the
-- | spec should test when non-pending.
pending'
  :: forall m
   . String
  -> m Unit
  -> Spec' m Unit
pending' name _ = pending name

-- | Create a spec with a description.
it
  :: forall m
   . String
  -> m Unit
  -> Spec' m Unit
it description tests = tell [It false description tests]

-- | Create a spec with a description and mark it as the only one to
-- | be run. (useful for quickly narrowing down on a single test)
itOnly
  :: forall m
   . String
  -> m Unit
  -> Spec' m Unit
itOnly description tests = tell [It true description tests]
