module Test.Spec where

import Control.Monad.Eff
import Control.Monad.Eff.Exception

type Name = String
data Result = Success
            | Failure Error
data Group = Describe Name [Group]
           | It Name Result
           | Pending Name

instance showResult :: Show Result where
  show Success = "Success"
  show (Failure err) = "Failure " ++ show err

instance eqResult :: Eq Result where
  (==) Success Success = true
  (==) (Failure _) (Failure _) = true
  (==) _ _ = false
  (/=) r1 r2 = not (r1 == r2)

instance showGroup :: Show Group where
  show (Describe name groups) = "Describe " ++ show name ++ " " ++ show groups
  show (It name result) = "It " ++ show name ++ " " ++ show result
  show (Pending name) = "Describe " ++ show name

instance eqGroup :: Eq Group where
  (==) (Describe n1 g1) (Describe n2 g2) = n1 == n2 && g1 == g2
  (==) (It n1 r1) (It n2 r2) = n1 == n2 && r1 == r2
  (==) (Pending n1) (Pending n2) = n1 == n2
  (==) _ _ = false
  (/=) r1 r2 = not (r1 == r2)
