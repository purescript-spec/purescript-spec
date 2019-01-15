module Test.Spec.Reporter.Console (consoleReporter) where

import Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer (class MonadWriter)
import Data.Array (all)
import Data.Foldable (for_, intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (uncurry)
import Effect.Exception as Error
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Execution(..))
import Test.Spec.Runner.Event as Event
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary
import Test.Spec.Tree (Path, Tree, parentSuite, parentSuiteName)

data RunningItem
  = RunningTest String (Maybe Result)
  | RunningPending String
  | RunningSuite String Boolean

derive instance runningItemGeneric :: Generic RunningItem _
instance runningItemShow :: Show RunningItem where show = genericShow


data PrintAction
  = PrintTest String Result
  | PrintPending String

derive instance printActionGeneric :: Generic PrintAction _
instance printActionShow :: Show PrintAction where show = genericShow

print
  :: forall s m
   . MonadState { lastPrintedSuitePath :: Maybe Path | s} m
  => MonadWriter String m
  => Path
  -> PrintAction
  -> m Unit
print path a = do
  for_ (parentSuite path) \suite -> do
    s <- get
    case s.lastPrintedSuitePath of
      Just p | p == suite.path -> pure unit
      _ -> do
        tellLn $ styled (Style.bold <> Style.magenta)
          $ intercalate " » " (parentSuiteName suite.path <> [suite.name])
        put s{lastPrintedSuitePath = Just suite.path}
  case a of
    PrintTest name (Success speed ms) -> do
      tellLn $ "  " <> styled Style.green "✓︎ " <> styled Style.dim name
    PrintTest name (Failure err) -> do
      tellLn $ "  " <> styled Style.red ("✗ " <> name <> ":")
      tellLn $ ""
      tellLn $ "  " <> styled Style.red (Error.message err)
    PrintPending name -> do
      tellLn $ "  " <> styled Style.cyan ("~ " <> name)

type State = { runningItem :: Map Path RunningItem, lastPrintedSuitePath :: Maybe Path}

initialState :: State
initialState = { runningItem: Map.empty, lastPrintedSuitePath: Nothing }

consoleReporter :: Reporter
consoleReporter = defaultReporter initialState case _ of
  Event.Suite Sequential path name ->
    pure unit
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
        print path $ PrintTest name res
  Event.Pending path name -> do
    {runningItem} <- get
    if Map.isEmpty runningItem
      then print path $ PrintPending name
      else modifyRunningItems $ Map.insert path $ RunningPending name
  Event.End results -> printSummary results
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
        RunningTest name (Just res) -> print path $ PrintTest name res
        RunningPending name -> print path $ PrintPending name
        _ -> pure unit
    where
      asArray = identity :: Array ~> Array
      runningItemIsFinished = case _ of
        RunningPending _ -> true
        RunningTest _ res -> isJust res
        RunningSuite _ finished -> finished


printSummary :: forall m. MonadWriter String m => Array (Tree Void Result) -> m Unit
printSummary = Summary.summarize >>> \(Count {passed, failed, pending}) -> do
  tellLn ""
  tellLn $ styled Style.bold "Summary"
  printPassedFailed passed failed
  printPending pending
  tellLn ""
  where
    printPassedFailed :: Int -> Int -> m Unit
    printPassedFailed p f = do
      let total = p + f
          testStr = pluralize "test" total
          amount = show p <> "/" <> (show total) <> " " <> testStr <> " passed"
          color = if f > 0 then Style.red else Style.dim
      tellLn $ styled color amount

    printPending :: Int -> m Unit
    printPending p
      | p > 0     = tellLn $ styled Style.yellow $ show p <> " " <> pluralize "test" p <> " pending"
      | otherwise = pure unit

    pluralize :: String -> Int -> String
    pluralize s 1 = s
    pluralize s _ = s <> "s"
