module Test.Spec.Assertions
  ( NoShow(..)
  , expectError
  , fail
  , shouldContain
  , shouldEqual
  , shouldNotContain
  , shouldNotEqual
  , shouldNotReturn
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  , class Printable
  , print
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, notElem, elem)
import Data.Newtype (class Newtype)
import Effect.Exception (Error, error)

fail :: forall m. MonadThrow Error m => String -> m Unit
fail = throwError <<< error

foreign import unsafeStringify :: forall a. a -> String

newtype NoShow a = NoShow a
instance Newtype (NoShow a) a
derive newtype instance (Eq a) => Eq (NoShow a)

class Printable a where
  print :: a -> String

instance Printable (NoShow a) where
  print = unsafeStringify
else instance Show a => Printable a where
  print = show

shouldEqual
  :: forall m t
   . MonadThrow Error m
  => Printable t
  => Eq t
  => t
  -> t
  -> m Unit
shouldEqual v1 v2 =
  when (v1 /= v2) $
    fail $ print v1 <> " ≠ " <> print v2

shouldNotEqual
  :: forall m t
   . MonadThrow Error m
  => Printable t
  => Eq t
  => t
  -> t
  -> m Unit
shouldNotEqual v1 v2 =
  when (v1 == v2) $
    fail $ print v1 <> " = " <> print v2

shouldSatisfy
  :: forall m t
   . MonadThrow Error m
  => Printable t
  => t
  -> (t -> Boolean)
  -> m Unit
shouldSatisfy v pred =
  unless (pred v) $
    fail $ print v <> " doesn't satisfy predicate"

shouldNotSatisfy
  :: forall m t
   . MonadThrow Error m
  => Printable t
  => t
  -> (t -> Boolean)
  -> m Unit
shouldNotSatisfy v pred =
  when (pred v) $
    fail $ print v <> " satisfies predicate, but should not"

shouldContain
  :: forall m f a
   . MonadThrow Error m
  => Printable a
  => Eq a
  => Printable (f a)
  => Foldable f
  => f a
  -> a
  -> m Unit
shouldContain c e =
  when (e `notElem` c) $
    fail $ (print e) <> " ∉ " <> (print c)

shouldNotContain
  :: forall m f a
   . MonadThrow Error m
  => Printable a
  => Eq a
  => Printable (f a)
  => Foldable f
  => f a
  -> a
  -> m Unit
shouldNotContain c e =
  when (e `elem` c) $
    fail $ (print e) <> " ∈ " <> (print c)

expectError
  :: forall m t
   . MonadError Error m
  => m t
  -> m Unit
expectError a = do
  e <- try a
  case e of
    Left _ -> pure unit
    Right _ -> throwError $ error "Expected error"

-- | Asserts that `m t` returns `t`
shouldReturn
  :: forall m t
   . MonadThrow Error m
  => Eq t
  => Printable t
  => m t
  -> t
  -> m Unit
shouldReturn ft t = ft >>= (_ `shouldEqual` t)

-- | Asserts that `m t` does not return `t`
shouldNotReturn
  :: forall m t
   . MonadThrow Error m
  => Eq t
  => Printable t
  => m t
  -> t
  -> m Unit
shouldNotReturn ft t = ft >>= (_ `shouldNotEqual` t)
