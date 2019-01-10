module Test.Spec
  ( Spec
  , SpecM
  , Tree(..)
  , Item(..)
  , SpecTree
  , ActionWith
  , class Example
  , evaluateExample
  , describe
  , describeOnly
  , parallel
  , sequential

  , pending
  , pending'
  , it
  , itOnly
  , countTests
  , Result(..)
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.State (execState)
import Control.Monad.State as State
import Control.Monad.Writer (WriterT(..), mapWriterT, runWriterT, tell)
import Data.Array (any)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, un)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.AVar as AVarEff
import Effect.Aff (Aff, error, throwError, try)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)

-- TODO remove this
anExample :: Spec Unit
anExample = do
  describe "foo" do
    it "asd" do
      pure unit
    it "asd" do
      pure unit
    before (pure 1) $ after (\a -> let x = a + 1 in pure unit) do
      it "asd" \num -> do
        x <- pure $ num + 1
        pure unit
      it "asdasdasd" \num -> do
        x <- pure $ num + 1
        pure unit
      beforeWith (const $ pure "asd") do
        it "asd" \str -> do
          z <- pure $ str <> "as"
          pure unit
        aroundWith (\e str -> let z = str <> "" in pure 1 >>= e <* pure unit) do
          it "asd" \num -> do
            z <- pure $ num + 1
            pure unit
      beforeWith (\num -> pure $ "asd" <> show (num + 1)) do
        it "asd" \str -> do
          z <- pure $ str <> "as"
          pure unit
    pure unit

data Tree c a
  = Node (Either String c) (Array (Tree c a))
  | Leaf String (Maybe a)

derive instance treeGeneric :: Generic (Tree c a) _
instance treeEq :: (Eq c, Eq a) => Eq (Tree c a) where eq = genericEq
instance treeShow :: (Show c, Show a) => Show (Tree c a) where show = genericShow


-- instance showGroup :: Show t => Show (Group t) where
--   show (SetExecution execution groups) = "SetExecution " <> show execution <> " " <> show groups
--   show (Describe only name groups) = "Describe " <> show only <> " " <> show name <> " " <> show groups
--   show (It only name test) = "It " <> show only <> " " <> show name <> " " <> show test
--   show (Pending name) = "Describe " <> show name

-- instance eqGroup :: Eq t => Eq (Group t) where
--   eq (SetExecution e1 g1) (SetExecution e2 g2) = e1 == e2 && g1 == g2
--   eq (Describe o1 n1 g1)  (Describe o2 n2 g2)  = o1 == o2 && n1 == n2 && g1 == g2
--   eq (It o1 n1 t1)        (It o2 n2 t2)        = o1 == o2 && n1 == n2 && t1 == t2
--   eq (Pending n1)         (Pending n2)         = n1 == n2
--   eq _                    _                    = false


instance treeFoldable :: Foldable (Tree c) where
  foldr f i (Leaf _ a) = maybe i (\a' -> f a' i) a
  foldr f i (Node _ as) = foldr (\a i' -> foldr f i' a) i as
  foldl f i (Leaf _ a) = maybe i (\a' -> f i a') a
  foldl f i (Node _ as) = foldl (\i' a -> foldl f i' a) i as
  foldMap f = foldMapDefaultL f

type ActionWith m a = a -> m Unit
type SpecTree m a = Tree (ActionWith m a) (Item m a)
newtype Item m a = Item
  { isFocused :: Boolean
  , isParallelizable :: Maybe Boolean
  , example :: (ActionWith m a -> m Unit) -> m Unit
  }

derive instance itemNewtype :: Newtype (Item m a) _

class Example t arg m | t -> arg, t -> m where
  evaluateExample :: t -> (ActionWith m arg -> m Unit) -> m Unit

instance exampleFunc :: Example (arg -> m Unit) arg m where
  evaluateExample :: (arg -> m Unit) -> (ActionWith m arg -> m Unit) -> m Unit
  evaluateExample t around' = around' t

else instance exampleMUnit :: Example (m Unit) Unit m where
  evaluateExample :: (m Unit) -> (ActionWith m Unit -> m Unit) -> m Unit
  evaluateExample t around' = around' $ \_ -> t


type Spec a = SpecM Identity Aff Unit a
type SpecM m g i a = WriterT (Array (SpecTree g i)) m a

bimapTree :: forall a b c d. (a -> b) -> (c -> d) -> Tree a c -> Tree b d
bimapTree g f = go
  where
    go spec = case spec of
      Node d xs -> Node (map g d) (map go xs)
      Leaf n item -> Leaf n (map f item)

mapSpecTree
  :: forall m g g' i a i'
   . Monad m
   => (SpecTree g i -> SpecTree g' i') 
   -> SpecM m g i a
   -> SpecM m g' i' a
mapSpecTree f (specs) = mapWriterT (map ((<$>) (map f))) specs

mapSpecItem
  :: forall m g g' a b r
   . Monad m
  => (ActionWith g a -> ActionWith g' b)
  -> (Item g a -> Item g' b)
  -> SpecM m g a r
  -> SpecM m g' b r
mapSpecItem g f = mapSpecTree (bimapTree g f)

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


-- | Count the total number of tests in a spec
countTests :: forall g i. Array (SpecTree g i) -> Int
countTests g = execState (for g go) 0
  where
  go (Node _ xs) = for_ xs go
  go (Leaf _ _) = State.modify_ (_ + 1)


-- ---------------------
-- --       DSL       --
-- ---------------------


-- | `focus` focuses all spec items of the given spec.
-- |
-- | Applying `focus` to a spec with focused spec items has no effect.
focus :: forall m g i a. Monad m => SpecM m g i a -> SpecM m g i a
focus test = WriterT do
  Tuple res xs <- runWriterT test
  pure $ Tuple res $ if any (any $ un Item >>> _.isFocused) xs
    then xs
    else map (bimapTree identity (\(Item r) -> Item r {isFocused = true})) xs


-- | Combine a group of specs into a described hierarchy.
describe
  :: forall m g i a
   . Monad m
  => String
  -> SpecM m g i a
  -> SpecM m g i a
describe name test = WriterT do
  Tuple res group <- runWriterT test
  pure $ Tuple res [Node (Left name) group]


-- | Combine a group of specs into a described hierarchy and mark it as the
-- | only group to actually be evaluated. (useful for quickly narrowing down
-- | on a set)
describeOnly
  :: forall m g i a
   . Monad m
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
parallel = mapSpecItem identity (setParallelizable true)

-- | marks all spec items of the given spec to be evaluated sequentially.
sequential
  :: forall m g i a
   . Monad m
  => SpecM m g i a
  -> SpecM m g i a
sequential = mapSpecItem identity (setParallelizable false)

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
   . Monad m
  => Example t arg g
  => String
  -> t
  -> SpecM m g arg Unit
itOnly = map focus <<< it

-- | Run a custom action before and/or after every spec item.
aroundWith
  :: forall m g i i' a
   . Monad m
  => (ActionWith g i -> ActionWith g i')
  -> SpecM m g i a
  -> SpecM m g i' a
aroundWith action = mapSpecItem action (modifyAroundAction action)


modifyAroundAction :: forall g a b. (ActionWith g a -> ActionWith g b) -> Item g a -> Item g b
modifyAroundAction action (Item item) = Item $ item
  { example = \aroundAction -> item.example (aroundAction <<< action)
  }

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
afterAll action spec = WriterT do
  Tuple res group <- runWriterT spec
  pure $ Tuple res [Node (Right action) group]

-- | Run a custom action after the last spec item.
afterAll_ :: forall m g i a. Monad m => g Unit -> SpecM m g i a -> SpecM m g i a
afterAll_ action = afterAll $ const action