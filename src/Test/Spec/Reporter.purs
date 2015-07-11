module Test.Spec.Reporter (
  Entry(..),
  Reporter(),
  collapse
  ) where

import Prelude

import Data.Array        (concatMap, cons)
import Data.Foldable     (foldl, intercalate)
import Control.Monad.Eff (Eff())

import qualified Test.Spec        as S
import           Test.Spec.Errors (errorMessage)

data Entry = Describe (Array S.Name)
           | It S.Name S.Result
           | Pending S.Name

instance eqEntry :: Eq Entry where
  eq (Describe n1) (Describe n2) = n1 == n2
  eq (It n1 S.Success) (It n2 S.Success) = n1 == n2
  eq (It n1 (S.Failure e1)) (It n2 (S.Failure e2)) =
    n1 == n2 && (errorMessage e1) == (errorMessage e2)
  eq (It n1 _) (It n2 _) = false
  eq (Pending n1) (Pending n2) = n1 == n2

instance showEntry :: Show Entry where
  show (Describe names) = "Describe \"" ++ (intercalate " » " names) ++ "\""
  show (It name S.Success) = "It \"" ++ name ++ "\" Success"
  show (It name (S.Failure err)) = "It \"" ++ name ++ "\" (Failure \"" ++ errorMessage err ++ "\")"
  show (Pending name) = "Pending \"" ++ name ++ "\""

type Reporter e = Array S.Group -> Eff e Unit

countDescribes :: Array Entry -> Int
countDescribes groups = foldl f 0 groups
  where f c (Describe _) = c + 1
        f c _ = c

collapse :: S.Group -> Array Entry
collapse (S.It name result) = [It name result]
collapse (S.Pending name) = [Pending name]
collapse (S.Describe name groups) =
  let sub = concatMap collapse groups
      prependName (Describe names) = Describe $ cons name names
      prependName e = e
      c = countDescribes sub
  in if c == 0 then cons (Describe [name]) sub
               else map prependName sub
