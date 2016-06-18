
module Test.Spec.Reporter.Console (consoleReporter) where

import Prelude

import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Console   (CONSOLE(), log)
import Control.Monad.Eff.Exception (message)
import Data.Array                  (concatMap)
import Data.Foldable               (intercalate, traverse_)

import Test.Spec          (Group(), Result(..))
import Test.Spec.Console  (withAttrs)
import Test.Spec.Reporter (Entry(..), Reporter(), collapse)
import Test.Spec.Summary  (Summary(..), summarize)

pluralize :: String -> Int -> String
pluralize s 1 = s
pluralize s _ = s <> "s"

printPassedFailed :: forall r. Int -> Int -> Eff (console :: CONSOLE | r) Unit
printPassedFailed p f = do
  let total = p + f
      testStr = pluralize "test" total
      amount = show p <> "/" <> (show total) <> " " <> testStr <> " passed"
      attrs = if f > 0 then [31] else [32]
  withAttrs attrs $ log amount

printPending :: forall r. Int -> Eff (console :: CONSOLE | r) Unit
printPending p
  | p > 0     = withAttrs [33] $ log (show p <> " " <> pluralize "test" p <> " pending")
  | otherwise = pure unit

printSummary' :: forall r. Summary -> Eff (console :: CONSOLE | r) Unit
printSummary' (Count passed failed pending) = do
  log ""
  withAttrs [1] $ log "Summary"
  printPassedFailed passed failed
  printPending pending
  log ""

printSummary :: forall r. Array Group -> Eff (console :: CONSOLE | r) Unit
printSummary groups = printSummary' $ summarize groups

printEntry :: forall r. Entry
           -> Eff (console :: CONSOLE | r) Unit
printEntry (It name Success) = do
  withAttrs [32] $ log $  "✓︎ " <> name
printEntry (Pending name) = do
  withAttrs [33] $ log $  "~ " <> name
printEntry (It name (Failure err)) = do
  withAttrs [31] $ log $ "✗ " <> name <> ":"
  log ""
  withAttrs [31] $ log $ "  " <> message err
printEntry (Describe n) = do
  log ""
  printNames n
  where printNames ns = withAttrs [1, 35] $ log $ intercalate " » " ns

consoleReporter :: forall e. Reporter (console :: CONSOLE | e)
consoleReporter groups = do
  traverse_ printEntry $ concatMap collapse groups
  printSummary groups
