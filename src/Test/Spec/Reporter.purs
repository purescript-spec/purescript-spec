module Test.Spec.Reporter (
  Entry(..),
  EntryPath,
  Entries,
  Reporter(),
  collapse,
  collapseAll,
  module Reexport
  ) where

import Prelude
import Test.Spec as S
import Data.Array (foldl)
import Data.Map   (unionWith, empty, singleton, Map)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Exception (message)
import Test.Spec (Name, Group, Result(Failure, Success))

import Test.Spec.Reporter.Spec    (specReporter)    as Reexport
import Test.Spec.Reporter.Console (consoleReporter) as Reexport
import Test.Spec.Reporter.Dot     (dotReporter)     as Reexport
import Test.Spec.Reporter.Tap     (tapReporter)     as Reexport

data Entry = It Name Result
           | Pending Name

instance eqEntry :: Eq Entry where
  eq (It n1 Success) (It n2 Success) = n1 == n2
  eq (It n1 (Failure e1)) (It n2 (Failure e2)) =
    n1 == n2 && (message e1) == (message e2)
  eq (Pending n1) (Pending n2) = n1 == n2
  eq _ _ = false

instance showEntry :: Show Entry where
  show (It name Success) = "It \"" <> name <> "\" Success"
  show (It name (Failure err)) = "It \"" <> name <> "\" (Failure \"" <> message err <> "\")"
  show (Pending name) = "Pending \"" <> name <> "\""

type Reporter e = (Array (Group Result)) -> Eff e Unit

type EntryPath = Array Name
type Entries = Map EntryPath (Array Entry)

collapseAt :: EntryPath -> Group Result -> Entries
collapseAt path group =
  case group of
    S.It _ name result -> singleton path [It name result]
    S.Pending name -> singleton path [Pending name]
    S.Describe _ name groups -> collapseAllAt (path <> [name]) groups

collapseAllAt :: EntryPath -> Array (Group Result) -> Entries
collapseAllAt path = foldl (unionWith (<>)) empty <<< map (collapseAt path)

collapse :: Group Result -> Entries
collapse = collapseAt []

collapseAll :: Array (Group Result) -> Entries
collapseAll = collapseAllAt []
