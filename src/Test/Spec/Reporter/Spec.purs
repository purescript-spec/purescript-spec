module Test.Spec.Reporter.Spec (specReporter) where

import Prelude

import Control.Monad.State (get, lift, put)
import Control.Monad.Writer (class MonadWriter, execWriter)
import Data.Array (all, length, null, replicate, sortBy)
import Data.Array as Array
import Data.Foldable (for_, sequence_)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Data.String (split, Pattern(..))
import Data.String.CodeUnits as CodeUnits
import Effect.Exception (Error)
import Effect.Exception as Error
import Test.Spec.Color (colored)
import Test.Spec.Color as Color
import Test.Spec.Console (logWriter, moveUpAndClearLine, tellLn)
import Test.Spec.Reporter.Base (defaultReporter, defaultSummary)
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed (Speed)
import Test.Spec.Tree (Path)

data RunningItem
  = RunningTest Path String (Maybe Res)
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

type Duration = Int
data Res = Success Speed Duration | Failure Error
instance showResult :: Show Res where
  show (Success speed duration) = "Success ()"
  show (Failure err) = "Failure (Error ...)"

specReporter :: Reporter
specReporter = defaultReporter initialState case _ of
  Event.Suite path name -> do
    modifyRunningItems (_ <> [RunningSuite path name false])
  Event.SuiteEnd path -> do
    modifyRunningItems $ map case _ of
      RunningSuite p n _ | p == path -> RunningSuite p n true
      a -> a
  Event.Test path name -> do
    modifyRunningItems (_ <> [RunningTest path name Nothing])
  Event.Pass path name speed ms -> do
    modifyRunningItems $ updateRunningTestResult path $ Success speed ms
  Event.Pending path name -> do
    modifyRunningItems (_ <> [PendingTest path name])
  Event.Fail path name err -> do
    modifyRunningItems $ updateRunningTestResult path $ Failure err
  Event.End results -> logWriter $ defaultSummary results
  Event.Start _ -> pure unit
  where
  updateRunningTestResult path res = map case _ of
    RunningTest p n _ | p == path -> RunningTest p n $ Just res
    a -> a

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
    for_ (sortByPath runningItems) case _ of
      PendingTest path name -> do
        tellLn $ (indent path) <> (colored Color.Pending $ "- " <> name)
      RunningTest path name Nothing -> do
        tellLn $ (indent path) <> colored Color.Pending "⥀ " <> name
      RunningTest path name (Just (Success _ _)) -> do
        tellLn $ (indent path) <> colored Color.Checkmark "✓︎ " <> colored Color.Pass name
      RunningTest path name (Just (Failure err)) -> do
        tellLn $ (indent path) <> colored Color.Fail ("✗ " <> name <> ":")
        tellLn $ ""
        tellLn $ (indent path) <> colored Color.Fail (Error.message err)
      RunningSuite path name _ -> tellLn $ (indent path) <> name
    where
      sortByPath = sortBy \a b -> on compare (runningItemPath) a b
      indent path = CodeUnits.fromCharArray $ Array.replicate (length path) ' '
