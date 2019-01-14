module Test.Spec
  ( Spec
  , SpecM
  , module Reexport
  , SpecTree
  , mapSpecTree

  , ComputationType(..)
  , hoistSpec

  , Result(..)
  , Duration

  , class Example
  , evaluateExample

  , parallel
  , sequential

  , class FocusWarning
  , focus
  , describeOnly
  , itOnly

  , describe
  , it
  , pending
  , pending'

  , aroundWith
  , around
  , around_

  , before
  , before_
  , beforeWith
  , beforeAll
  , beforeAll_

  , after
  , after_
  , afterAll
  , afterAll_
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Writer (WriterT, mapWriterT, tell)
import Data.Array (any)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Function (applyFlipped)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, un)
import Effect.AVar (AVar)
import Effect.AVar as AVarEff
import Effect.Aff (Aff, error, throwError, try)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Prim.TypeError (class Warn, Text)
import Test.Spec.Speed (Speed)
import Test.Spec.Tree (ActionWith, Item(..), Tree(..)) as Reexport
import Test.Spec.Tree (ActionWith, Item(..), Tree(..), bimapTree, modifyAroundAction)


type Spec a = SpecM Identity Aff Unit a
type SpecM m g i a = WriterT (Array (SpecTree g i)) m a

type SpecTree m a = Tree (ActionWith m a) (Item m a)

mapSpecTree
  :: forall m g g' i a i'
   . Monad m
   => (SpecTree g i -> SpecTree g' i')
   -> SpecM m g i a
   -> SpecM m g' i' a
mapSpecTree f = mapWriterT $ map $ map $ map f

data ComputationType = CleanUpWithContext (Array String) | TestWithName (NonEmptyArray String)

hoistSpec :: forall m i a b. Monad m => (ComputationType -> a ~> b) -> SpecM m a i ~> SpecM m b i
hoistSpec f = mapSpecTree $ bimapTree onCleanUp onTest
  where
    onCleanUp :: Array String -> (ActionWith a i) -> ActionWith b i
    onCleanUp name around' = \i -> f (CleanUpWithContext name) (around' i)
    onTest :: NonEmptyArray String -> Item a i -> Item b i
    onTest name = over Item \item ->
      let
        e :: ((i -> b Unit) -> b Unit) -> b Unit
        e g = g (f (TestWithName name) <<< item.example <<< applyFlipped)
      in item { example = e }


class Example t arg m | t -> arg, t -> m where
  evaluateExample :: t -> (ActionWith m arg -> m Unit) -> m Unit

instance exampleFunc :: Example (arg -> m Unit) arg m where
  evaluateExample :: (arg -> m Unit) -> (ActionWith m arg -> m Unit) -> m Unit
  evaluateExample t around' = around' t

else instance exampleMUnit :: Example (m Unit) Unit m where
  evaluateExample :: (m Unit) -> (ActionWith m Unit -> m Unit) -> m Unit
  evaluateExample t around' = around' $ \_ -> t


type Duration = Int
data Result
  = Success Speed Duration
  | Failure Error

instance showResult :: Show Result where
  show (Success speed duration ) = "Success (" <> show speed <> " " <> show duration <> ")"
  show (Failure err) = "Failure (Error ...)"

instance eqResult :: Eq Result where
  eq (Success s1 d1) (Success s2 d2) = s1 == s2 && d1 == d2
  eq (Failure _) (Failure _) = true
  eq _ _ = false


-- | Nullary class used to raise a custom warning for the focusing functions.
class FocusWarning

instance warn :: Warn (Text "Test.Spec.focus usage") => FocusWarning

-- ---------------------
-- --       DSL       --
-- ---------------------

-- | `focus` focuses all spec items of the given spec.
-- |
-- | Applying `focus` to a spec with focused spec items has no effect.
focus :: forall m g i a. FocusWarning => Monad m => SpecM m g i a -> SpecM m g i a
focus = mapWriterT $ map $ map \xs ->
  if any (any $ un Item >>> _.isFocused) xs
    then xs
    else map (bimap identity (\(Item r) -> Item r {isFocused = true})) xs


-- | Combine a group of specs into a described hierarchy.
describe
  :: forall m g i a
   . Monad m
  => String
  -> SpecM m g i a
  -> SpecM m g i a
describe name = mapWriterT $ map $ map \group -> [Node (Left name) group]


-- | Combine a group of specs into a described hierarchy and mark it as the
-- | only group to actually be evaluated. (useful for quickly narrowing down
-- | on a set)
describeOnly
  :: forall m g i a
   . FocusWarning
  => Monad m
  => String
  -> SpecM m g i a
  -> SpecM m g i a
describeOnly = map focus <<< describe

-- | marks all spec items of the given spec to be safe for parallel evaluation.
parallel
  :: forall m g i a
   . Monad m
  => SpecM m g i a
  -> SpecM m g i a
parallel = mapSpecTree $ bimap identity (setParallelizable true)

-- | marks all spec items of the given spec to be evaluated sequentially.
sequential
  :: forall m g i a
   . Monad m
  => SpecM m g i a
  -> SpecM m g i a
sequential = mapSpecTree $ bimap identity (setParallelizable false)

setParallelizable :: forall g a. Boolean -> Item g a -> Item g a
setParallelizable value = over Item \i -> i{isParallelizable = i.isParallelizable <|> Just value}

-- | Create a pending spec.
pending
  :: forall m g i
   . Monad m
  => String
  -> SpecM m g i Unit
pending name = tell [Leaf name Nothing]

-- | Create a pending spec with a body that is ignored by
-- | the runner. It can be useful for documenting what the
-- | spec should test when non-pending.
pending'
  :: forall m g i
   . Monad m
  => String
  -> g Unit
  -> SpecM m g i Unit
pending' name _ = pending name

-- | Create a spec with a description.
it
  :: forall m t arg g
   . Monad m
  => Example t arg g
  => String
  -> t
  -> SpecM m g arg Unit
it name test = tell
  [ Leaf name $ Just $ Item
      { isParallelizable: Nothing
      , isFocused: false
      , example: evaluateExample test
      }
  ]

-- | Create a spec with a description and mark it as the only one to
-- | be run. (useful for quickly narrowing down on a single test)
itOnly
  :: forall m t arg g
   . FocusWarning
  => Monad m
  => Example t arg g
  => String
  -> t
  -> SpecM m g arg Unit
itOnly = map focus <<< it


-- ---------------------
-- --      HOOKS      --
-- ---------------------

-- | Run a custom action before and/or after every spec item.
aroundWith
  :: forall m g i i' a
   . Monad m
  => (ActionWith g i -> ActionWith g i')
  -> SpecM m g i a
  -> SpecM m g i' a
aroundWith action = mapSpecTree $ bimap action (modifyAroundAction action)

-- | Run a custom action before and/or after every spec item.
around_ :: forall m g i a. Monad m => (g Unit -> g Unit) -> SpecM m g i a -> SpecM m g i a
around_ action = aroundWith $ \e a -> action (e a)

-- | Run a custom action after every spec item.
after :: forall m g e f i a. Monad m => MonadBracket e f g => ActionWith g i -> SpecM m g i a -> SpecM m g i a
after action = aroundWith $ \e x -> e x `finally` action x
  where
  finally :: forall x. g x -> g Unit -> g x
  finally act fin = bracket (pure unit) (\_ _ -> fin) (const act)

-- | Run a custom action after every spec item.
after_ :: forall m g e f i a. Monad m => MonadBracket e f g => g Unit -> SpecM m g i a -> SpecM m g i a
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item.
around :: forall m g i a. Monad m => (ActionWith g i -> g Unit) -> SpecM m g i a -> SpecM m g Unit a
around action = aroundWith $ \e _ -> action e

-- | Run a custom action before every spec item.
before :: forall m g i a. Monad m => Monad g => g i -> SpecM m g i a -> SpecM m g Unit a
before action = around (action >>= _)

-- | Run a custom action before every spec item.
before_ :: forall m g i a. Monad m => Monad g => g Unit -> SpecM m g i a -> SpecM m g i a
before_ action = around_ (action *> _)

-- | Run a custom action before every spec item.
beforeWith :: forall m g i i' a. Monad m => Monad g => (i' -> g i) -> SpecM m g i a -> SpecM m g i' a
beforeWith action = aroundWith $ \e x -> action x >>= e

-- | Run a custom action before the first spec item.
beforeAll :: forall m g i a. MonadEffect m => MonadAff g => MonadError Error g => g i -> SpecM m g i a -> SpecM m g Unit a
beforeAll action spec = do
  var <- liftEffect $ AVarEff.new MEmpty
  before (memoize var action) spec

-- | Run a custom action before the first spec item.
beforeAll_ :: forall m g i a. MonadEffect m => MonadAff g => MonadError Error g => g Unit -> SpecM m g i a -> SpecM m g i a
beforeAll_ action spec = do
  var <- liftEffect $ AVarEff.new MEmpty
  before_ (memoize var action) spec

data Memoized a
  = MEmpty
  | MMemoized a
  | MFailed Error

memoize :: forall a m. MonadAff m => MonadError Error m => AVar (Memoized a) -> m a -> m a
memoize var action = do
  liftAff (AVar.take var) >>= case _ of
    MFailed x -> throwError $ error "exception in beforeAll-hook (see previous failure)"
    MMemoized x -> pure x <* (liftAff $ AVar.put (MMemoized x) var)
    MEmpty -> do
      res <- try action
      liftAff $ AVar.put (either MFailed MMemoized res) var
      either throwError pure res

-- | Run a custom action after the last spec item.
afterAll :: forall m g i a. Monad m => ActionWith g i -> SpecM m g i a -> SpecM m g i a
afterAll action = mapWriterT $ map $ map \group -> [Node (Right action) group]

-- | Run a custom action after the last spec item.
afterAll_ :: forall m g i a. Monad m => g Unit -> SpecM m g i a -> SpecM m g i a
afterAll_ action = afterAll $ const action
