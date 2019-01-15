module Test.Spec.Reporter.Spec (specReporter) where

import Prelude

import Control.Monad.State (class MonadState, get, modify, put)
import Control.Monad.Writer (class MonadWriter)
import Data.Array (all, length)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (uncurry)
import Test.Spec.Color (colored)
import Test.Spec.Color as Color
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (defaultReporter, defaultSummary)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Execution(..))
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed
import Test.Spec.Tree (Path)

data RunningItem
  = RunningTest String (Maybe Result)
  | RunningPending String
  | RunningSuite String Boolean

derive instance runningItemGeneric :: Generic RunningItem _
instance runningItemShow :: Show RunningItem where show = genericShow

type State = { runningItem :: Map Path RunningItem, numFailures :: Int }

initialState :: State
initialState = { runningItem: Map.empty, numFailures: 0}

data PrintAction
  = PrintSuite Path String
  | PrintTest Path String Result
  | PrintPending Path String

derive instance printActionGeneric :: Generic PrintAction _
instance printActionShow :: Show PrintAction where show = genericShow

print
  :: forall s m
   . MonadState { numFailures :: Int | s } m
  => MonadWriter String m
  => PrintAction
  -> m Unit
print = case _ of
  PrintSuite path name -> do
    tellLn $ indent path <> name
  PrintTest path name (Success speed ms) -> do
    let
      speedDetails = case speed of
        Speed.Fast -> ""
        _ -> colored (Speed.toColor speed) $ " (" <> show ms <> "ms)"
    tellLn $ (indent path) <> colored Color.Checkmark "✓︎ " <> colored Color.Pass name <> speedDetails
  PrintTest path name (Failure err) -> do
    {numFailures} <- modify \s -> s{numFailures = s.numFailures +1}
    tellLn $ (indent path) <> colored Color.Fail (show numFailures <> ") " <> name)
  PrintPending path name -> do
    tellLn $ (indent path) <> (colored Color.Pending $ "- " <> name)
  where
    indent path = CodeUnits.fromCharArray $ Array.replicate (length path) ' '

specReporter :: Reporter
specReporter = defaultReporter initialState case _ of
  Event.Suite Sequential path name -> do
    print $ PrintSuite path name
  Event.Suite Parallel path name -> do
    modifyRunningItems $ Map.insert path $ RunningSuite name false
  Event.SuiteEnd path -> do
    modifyRunningItems $ flip Map.update path case _ of
      RunningSuite n _ -> Just $ RunningSuite n true
      a -> Nothing
  Event.Test Sequential path name -> do
    pure unit
  Event.Test Parallel path name -> do
    modifyRunningItems $ Map.insert path $ RunningTest name Nothing
  Event.TestEnd path name res -> do
    {runningItem} <- get
    case Map.lookup path runningItem of
      Just (RunningTest n _) ->
        modifyRunningItems $ Map.insert path $ RunningTest n $ Just res
      _ ->
        print $ PrintTest path name res
  Event.Pending path name -> do
    {runningItem} <- get
    if Map.isEmpty runningItem
      then print $ PrintPending path name
      else modifyRunningItems $ Map.insert path $ RunningPending name
  Event.End results -> defaultSummary results
  Event.Start _ -> pure unit
  where
  modifyRunningItems f = do
    s <- get
    let
      nextRunningItems = f s.runningItem
      allFinished = all runningItemIsFinished nextRunningItems
    put s{runningItem = if allFinished then Map.empty else nextRunningItems}

    when allFinished do
      for_ (asArray $ Map.toUnfoldable nextRunningItems) $ uncurry \path -> case _ of
        RunningTest name (Just res) -> print $ PrintTest path name res
        RunningPending name -> print $ PrintPending path name
        RunningSuite name true -> print $ PrintSuite path name
        _ -> pure unit
    where
      asArray = identity :: Array ~> Array
      runningItemIsFinished = case _ of
        RunningPending _ -> true
        RunningTest _ res -> isJust res
        RunningSuite _ finished -> finished
