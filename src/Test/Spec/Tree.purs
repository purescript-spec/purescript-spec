module Test.Spec.Tree
  ( ActionWith
  , Item(..)
  , Path
  , PathItem(..)
  , Tree(..)
  , annotateWithPaths
  , bimapTreeWithPaths
  , countTests
  , discardUnfocused
  , filterTree
  , filterTrees
  , isAllParallelizable
  , mapTreeAnnotations
  , modifyAroundAction
  , parentSuite
  , parentSuiteName
  )
  where

import Prelude

import Control.Monad.State (execState)
import Control.Monad.State as State
import Data.Array (mapMaybe, mapWithIndex, snoc)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either, either)
import Data.Foldable (class Foldable, all, foldMapDefaultL, foldl, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un)
import Data.Traversable (for, for_)
import Data.Tuple.Nested (type (/\), (/\))

data Tree n c a
  = Node (Either n c) (Array (Tree n c a))
  | Leaf n (Maybe a)

derive instance Functor (Tree n c)

instance (Show c, Show a, Show n) => Show (Tree n c a) where
  show (Node nc xs) = "(Node " <> show nc <> " " <> show xs <> ")"
  show (Leaf name t) = "(Leaf " <> show name <> " " <> show t <> ")"

instance (Eq c, Eq a, Eq name) => Eq (Tree name c a) where
  eq (Node nc1 xs1) (Node nc2 xs2) = nc1 == nc2 && xs1 == xs2
  eq (Leaf n1 t1) (Leaf n2 t2) = n1 == n2 && t1 == t2
  eq _ _ = false

bimapTreeWithPaths :: ∀ a b c d n. (Array n -> a -> b) -> (NonEmptyArray n -> c -> d) -> Tree n a c -> Tree n b d
bimapTreeWithPaths g f = go []
  where
  go :: Array n -> Tree n a c -> Tree n b d
  go namePath spec = case spec of
    Node d xs ->
      let
        namePath' = either (snoc namePath) (const namePath) d
      in
        Node (map (g namePath') d) (map (go namePath') xs)
    Leaf n item -> Leaf n (map (f $ NEA.snoc' namePath n) item)

instance Bifunctor (Tree n) where
  bimap g f tree = case tree of
    Node c cs -> Node (g <$> c) $ bimap g f <$> cs
    Leaf n a -> Leaf n $ f <$> a

instance Foldable (Tree n c) where
  foldr f i (Leaf _ a) = maybe i (\a' -> f a' i) a
  foldr f i (Node _ as) = foldr (\a i' -> foldr f i' a) i as
  foldl f i (Leaf _ a) = maybe i (\a' -> f i a') a
  foldl f i (Node _ as) = foldl (\i' a -> foldl f i' a) i as
  foldMap f = foldMapDefaultL f

type ActionWith m a = a -> m Unit

newtype Item m a = Item
  { isFocused :: Boolean
  , isParallelizable :: Maybe Boolean
  , example :: (ActionWith m a -> m Unit) -> m Unit
  }

derive instance itemNewtype :: Newtype (Item m a) _

instance itemShow :: Show (Item m a) where
  show (Item { isFocused, isParallelizable }) =
    "Item (" <> show { isFocused, isParallelizable, example: "Function" } <> ")"

instance itemEq :: Eq (Item m a) where
  eq (Item a) (Item b) =
    a.isFocused == b.isFocused && a.isParallelizable == b.isParallelizable

annotateWithPaths :: ∀ c a. Array (Tree String c a) -> Array (Tree (String /\ Path) c a)
annotateWithPaths = mapWithIndex $ go []
  where
    go path index = case _ of
      Node c children ->
        let name = either Just (const Nothing) c
            nextPath = path <> [PathItem { index, name }]
        in
          Node (c # lmap (_ /\ path)) (mapWithIndex (go nextPath) children)

      Leaf name item ->
        Leaf (name /\ path) item

mapTreeAnnotations :: ∀ n m c a. (n -> m) -> Tree n c a -> Tree m c a
mapTreeAnnotations f = case _ of
  Node c cs -> Node (lmap f c) $ mapTreeAnnotations f <$> cs
  Leaf n a -> Leaf (f n) a

filterTrees :: ∀ n c a. (n -> Maybe a -> Boolean) -> Array (Tree n c a) -> Array (Tree n c a)
filterTrees f = mapMaybe $ filterTree f

filterTree :: ∀ n c a. (n -> Maybe a -> Boolean) -> Tree n c a -> Maybe (Tree n c a)
filterTree f = case _ of
  Node c children ->
    case mapMaybe (filterTree f) children of
      [] -> Nothing
      cs -> Just $ Node c cs

  Leaf n a
    | f n a -> Just $ Leaf n a
    | otherwise -> Nothing

-- | Count the total number of tests in a spec
countTests :: ∀ n c t. Array (Tree n c t) -> Int
countTests g = execState (for g go) 0
  where
  go (Node _ xs) = for_ xs go
  go (Leaf _ _) = State.modify_ (_ + 1)

-- | Return true if all items in the tree are parallelizable
isAllParallelizable :: ∀ n c m a. Tree n c (Item m a) -> Boolean
isAllParallelizable = case _ of
  Node _ xs -> all isAllParallelizable xs
  Leaf _ x -> x == Nothing || (x >>= un Item >>> _.isParallelizable) == Just true

-- | If there is at least one focused element, all paths which don't
-- | lead to a focused element will be remove. otherwise input will
-- | be returned as unchanged.
discardUnfocused :: ∀ n c m a. Array (Tree n c (Item m a)) -> Array (Tree n c (Item m a))
discardUnfocused ts =
  case filterTrees isFocused ts of
    [] -> ts
    r -> r
  where
    isFocused _ (Just (Item i)) = i.isFocused
    isFocused _ Nothing = false

-- | Modify around action of an Item
modifyAroundAction :: ∀ g a b. (ActionWith g a -> ActionWith g b) -> Item g a -> Item g b
modifyAroundAction action (Item item) = Item $ item
  { example = \aroundAction -> item.example (aroundAction <<< action)
  }

newtype PathItem = PathItem { index :: Int, name :: Maybe String }

derive instance newtypePathItem :: Newtype PathItem _
derive newtype instance showIdTerm :: Show PathItem
derive newtype instance pathItemEq :: Eq PathItem
derive newtype instance pathItemOrd :: Ord PathItem

type Path = Array PathItem

parentSuiteName :: Path -> Array String
parentSuiteName = mapMaybe (un PathItem >>> _.name)

parentSuite :: Path -> Maybe { path :: Path, name :: String }
parentSuite = flip foldr Nothing case _, _ of
  PathItem { name: Just name }, Nothing -> Just { path: [], name }
  PathItem { name: Nothing }, Nothing -> Nothing
  p, Just acc -> Just acc { path = [ p ] <> acc.path }
