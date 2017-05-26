module Test.Spec (
  Name(..),
  Only(..),
  Result(..),
  Group(..),
  Spec,
  SpecWith,
  SpecEffects,
  describe,
  describeOnly,
  pending,
  pending',
  it,
  itOnly,
  -- beforeEach,
  -- afterEach,
  aroundEach,
  collect,
  countTests,
  class Example,
  eval
  ) where

import Prelude
import Control.Monad.State as State
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Aff.AVar (AVAR, AVar, peekVar)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.Console as Console
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.State (State, modify, execState, runState)
import Data.Traversable (for, for_)
import Data.Tuple (snd)
import Unsafe.Coerce (unsafeCoerce)

type Name = String
type Only = Boolean

data Group t
  = Describe Only Name (Array (Group t))
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
  show (Describe only name groups) = "Describe " <> show only <> " " <> show name <> " " <> show groups
  show (It only name test) = "It " <> show only <> " " <> show name <> " " <> show test
  show (Pending name) = "Describe " <> show name

instance eqGroup :: Eq t => Eq (Group t) where
  eq (Describe o1 n1 g1) (Describe o2 n2 g2) = o1 == o2 && n1 == n2 && g1 == g2
  eq (It o1 n1 t1)       (It o2 n2 t2)       = o1 == o2 && n1 == n2 && t1 == t2
  eq (Pending n1)        (Pending n2)        = n1 == n2
  eq _                   _                   = false

instance functorGroup :: Functor Group where
  map f (It o n t) = It o n (f t)
  map f (Describe o n ss) = Describe o n ((f <$> _) <$> ss)
  map _ (Pending n) = Pending n

class Example eff arg fun
  | arg -> eff
  , fun -> arg
  , fun -> eff
  where
  eval :: fun -> AVar arg -> Aff eff Unit

instance exampleFunc :: Example eff arg (arg -> Aff eff Unit) where
  eval f var = do
    arg <- unsafeCoerceAff $ peekVar var -- TODO: make this safe
    f arg

instance exampleAff :: Example eff a (Aff eff Unit) where
  eval u _ = u

-- Specifications with unevaluated tests.
type SpecEffects e =  ( console :: CONSOLE
                      , timer   :: TIMER
                      , avar    :: AVAR
                      | e)
type Spec eff r = SpecWith (Aff eff Unit) r
type SpecWith a r = State (Array (Group a)) r

collect
  :: ∀ t r
   . State (Array (Group t)) r
  -> Array (Group t)
collect r = snd $ runState r []

-- | Count the total number of tests in a spec
countTests :: forall r. SpecWith r Unit -> Int
countTests spec = execState (for (collect spec) go) 0
  where
  go (Describe _ _ xs) = for_ xs go
  go _ = State.modify (_ + 1)

---------------------
--       DSL       --
---------------------

-- | Combine a group of specs into a described hierarchy that either has the
-- |"only" modifier applied or not.
_describe
  :: ∀ eff arg fun
   . Example eff arg fun
  => Boolean
  -> String
  -> SpecWith fun Unit
  -> SpecWith fun Unit
_describe only name its = modify (_ <> [Describe only name (collect its)])

-- | Combine a group of specs into a described hierarchy.
describe
  :: ∀ eff arg fun
   . Example eff arg fun
  => String
  -> SpecWith fun Unit
  -> SpecWith fun Unit
describe = _describe false

-- | Combine a group of specs into a described hierarchy and mark it as the
-- | only group to actually be evaluated. (useful for quickly narrowing down
-- | on a set)
describeOnly
  :: ∀ eff arg fun
   . Example eff arg fun
  => String
  -> SpecWith fun Unit
  -> SpecWith fun Unit
describeOnly = _describe true

-- | Create a pending spec.
pending
  :: ∀ eff arg fun
   . Example eff arg fun
  => String
  -> SpecWith fun Unit
pending name = modify $ \p -> p <> [Pending name]

-- | Create a pending spec with a body that is ignored by
-- | the runner. It can be useful for documenting what the
-- | spec should test when non-pending.
pending'
  :: ∀ eff arg fun
   . Example eff arg fun
  => String
  -> fun
  -> SpecWith fun Unit
pending' name _ = pending name

-- | Create a spec with a description.
it
  :: ∀ eff arg fun
   . Example eff arg fun
  => String
  -> fun
  -> SpecWith fun Unit
it description example = modify (_ <> [It false description example])

-- | Create a spec with a description and mark it as the only one to
-- | be run. (useful for quickly narrowing down on a single test)
itOnly
  :: ∀ eff arg fun
   . Example eff arg fun
  => String
  -> fun
  -> SpecWith fun Unit
itOnly description example = modify (_ <> [It true description example])

-- | Run an effectful computation around each test, passing the result to
-- | the test and cleaning up afterwards
aroundEach
  :: ∀ eff1 eff2 eff3 eff4 fun1 fun2 arg1
   . Example eff3 arg1 fun1
  => Example eff4 Unit fun2
  => Aff eff1 arg1
  -> (arg1 -> Aff eff2 Unit)
  -> SpecWith fun1 Unit
  -> SpecWith fun2 Unit
aroundEach before after spec = modify $ const $
  let groups = collect spec
   in groups <#> \group ->
        group <#> \example -> do
          unsafeCoerceAff $ Console.log "..." --
