module Test.Spec.Reporter (
  Entry(..),
  Reporter(),
  collapse,
  showAssertionError
  ) where

import Debug.Trace
import Data.String
import Data.Array (map, concatMap)
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Extras
import qualified Test.Spec as S
import Test.Spec.Console
import Test.Spec.Summary
import Test.Spec.Reporter

foreign import showAssertionError
  """
  function showAssertionError(err) {
    return err.message;
  }
  """ :: Error -> String

data Entry = Describe [S.Name]
           | It S.Name S.Result
           | Pending S.Name

instance eqEntry :: Eq Entry where
  (==) (Describe n1) (Describe n2) = n1 == n2
  (==) (It n1 S.Success) (It n2 S.Success) = n1 == n2
  (==) (It n1 (S.Failure e1)) (It n2 (S.Failure e2)) =
    n1 == n2 && (showAssertionError e1) == (showAssertionError e2)
  (==) (It n1 _) (It n2 _) = false
  (==) (Pending n1) (Pending n2) = n1 == n2
  (/=) e1 e2 = not (e1 == e2)

instance showEntry :: Show Entry where
  show (Describe names) = "Describe \"" ++ (joinWith " » " names) ++ "\""
  show (It name S.Success) = "It \"" ++ name ++ "\" Success"
  show (It name (S.Failure err)) = "It \"" ++ name ++ "\" (Failure \"" ++ showAssertionError err ++ "\")"
  show (Pending name) = "Pending \"" ++ name ++ "\""

type Reporter e = ([S.Group] -> Eff e Unit)

countDescribes :: [Entry] -> Number
countDescribes groups = foldl f 0 groups
  where f c (Describe _) = c + 1
        f c _ = c

collapse :: S.Group -> [Entry]
collapse (S.It name result) = [It name result]
collapse (S.Pending name) = [Pending name]
collapse (S.Describe name groups) =
  let sub = concatMap collapse groups
      prependName (Describe names) = Describe (name : names)
      prependName e = e
      c = countDescribes sub
  in if c == 0 then (Describe [name]) : sub
               else map prependName sub
