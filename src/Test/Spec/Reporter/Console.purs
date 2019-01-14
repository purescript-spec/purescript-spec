module Test.Spec.Reporter.Console (consoleReporter) where

import Prelude

import Control.Monad.State (get, lift, put)
import Control.Monad.Writer (class MonadWriter, execWriter, tell)
import Data.Array (all, foldMap, groupBy, length, mapMaybe, null, replicate, sortBy)
import Data.Array.NonEmpty as NEA
import Data.Foldable (for_, intercalate, sequence_)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Data.String (split, Pattern(..))
import Effect.Exception as Error
import Test.Spec.Result (Result(..))
import Test.Spec.Color (colored)
import Test.Spec.Color as Color
import Test.Spec.Console (moveUpAndClearLine, logWriter, withAttrs)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary
import Test.Spec.Tree (Tree, Path, parentSuiteName, removeLastIndex)

data RunningItem
  = RunningTest Path String (Maybe Result)
  | PendingTest Path String
  | RunningSuite Path String Boolean

runningItemPath :: RunningItem -> Path
runningItemPath = case _ of
  RunningTest p _ _ -> p
  PendingTest p _ -> p
  RunningSuite p _ _ -> p

derive instance runningItemGeneric :: Generic RunningItem _
instance runningItemShow :: Show RunningItem where show = genericShow

initialState :: Array RunningItem
initialState = []

consoleReporter :: Reporter
consoleReporter = defaultReporter initialState case _ of
  Event.Suite path name -> do
    modifyRunningItems (_ <> [RunningSuite path name false])
  Event.SuiteEnd path -> do
    modifyRunningItems $ map case _ of
      RunningSuite p n _ | p == path -> RunningSuite p n true
      a -> a
  Event.Test path name -> do
    modifyRunningItems (_ <> [RunningTest path name Nothing])
  Event.TestEnd path name res -> do
    modifyRunningItems $ map case _ of
      RunningTest p n _ | p == path -> RunningTest p n $ Just res
      a -> a
  Event.Pending path name -> do
    modifyRunningItems (_ <> [PendingTest path name])
  Event.End results -> logWriter $ printSummary results
  Event.Start _ -> pure unit
  where

  modifyRunningItems f = do
    currentRunningItems <- get
    let nextRunningItems = f currentRunningItems
    put if allRunningItemsAreFinished nextRunningItems then [] else nextRunningItems
    unless (null currentRunningItems) do
      let c = lineCount $ execWriter $ writeRunningItems currentRunningItems
      lift $ sequence_ $ replicate c moveUpAndClearLine
    logWriter $ writeRunningItems nextRunningItems
    where
      lineCount str = length (split (Pattern "\n") str) - 1
      allRunningItemsAreFinished = all case _ of
        PendingTest _ _ -> true
        RunningTest _ _ res -> isJust res
        RunningSuite _ _ finished -> finished

  writeRunningItems :: forall m. MonadWriter String m => Array RunningItem -> m Unit
  writeRunningItems runningItems = do
    for_ (groupeBySuite $ sortByPath $ removeSuitesNodes runningItems) \g -> do
      logCrumbs (parentSuiteName $ runningItemPath $ NEA.head g)
      for_ (NEA.toArray g ) case _ of
        PendingTest _ name -> tell $ asLine
          [ "  " <> (colored Color.Pending $ "~ " <> name)
          ]
        RunningTest _ name Nothing -> tell $ asLine
          [ "  " <> colored Color.Pending "⥀ " <> name
          ]
        RunningTest _ name (Just (Success _ _)) -> tell $ asLine
          [ "  " <> colored Color.Checkmark "✓︎ " <> colored Color.Pass name
          ]
        RunningTest _ name (Just (Failure err)) -> tell $ asLine
          [ "  " <> colored Color.Fail ("✗ " <> name <> ":")
          , ""
          , "  " <> colored Color.Fail (Error.message err)
          ]
        RunningSuite _ _ _ -> pure unit
    where
      removeSuitesNodes = mapMaybe case _ of
        RunningSuite _ _ _ -> Nothing
        a -> Just a
      sortByPath = sortBy \a b -> on compare (runningItemPath) a b
      groupeBySuite = groupBy (on (==) $ runningItemPath >>> removeLastIndex)


printSummary :: forall m. MonadWriter String m => Array (Tree Void Result) -> m Unit
printSummary = Summary.summarize >>> \(Count {passed, failed, pending}) -> do
  tell $ asLine [""]
  withAttrs [1] $ tell $ asLine ["Summary"]
  printPassedFailed passed failed
  printPending pending
  tell $ asLine [""]
  where
    printPassedFailed :: Int -> Int -> m Unit
    printPassedFailed p f = do
      let total = p + f
          testStr = pluralize "test" total
          amount = show p <> "/" <> (show total) <> " " <> testStr <> " passed"
          attrs = if f > 0 then [31] else [32]
      withAttrs attrs $ tell $ asLine [amount]

    printPending :: Int -> m Unit
    printPending p
      | p > 0     = withAttrs [33] $ tell $ asLine [show p <> " " <> pluralize "test" p <> " pending"]
      | otherwise = pure unit

logCrumbs :: forall m. MonadWriter String m => Array String -> m Unit
logCrumbs crumbs = withAttrs [1, 35] $ tell $ asLine [intercalate " » " crumbs]

asLine :: Array String -> String
asLine = foldMap (_ <> "\n")

pluralize :: String -> Int -> String
pluralize s 1 = s
pluralize s _ = s <> "s"
