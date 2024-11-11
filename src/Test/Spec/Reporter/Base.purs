module Test.Spec.Reporter.Base
  ( defaultSummary
  , defaultReporter
  , defaultUpdate
  , RunningItem(..)
  ) where

import Prelude

import Control.Monad.State (StateT, evalStateT, execStateT, get, gets, put)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadWriter, Writer, runWriter)
import Data.Either (Either(..))
import Data.Foldable (all, for_, intercalate, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Exception as Error
import Pipes (await, yield)
import Pipes.Core (Pipe)
import Test.Spec (Tree)
import Test.Spec as S
import Test.Spec.Console (tellLn)
import Test.Spec.Console as Console
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Event)
import Test.Spec.Runner.Event as Event
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary
import Test.Spec.Tree (Name, TestLocator, annotatedWithPaths, parentSuiteName)


defaultSummary :: forall m
   . MonadWriter String m
  => Array (Tree String Void Result)
  -> m Unit
defaultSummary xs = do
  case Summary.summarize xs of
    (Count {passed, failed, pending}) -> do
      when (passed  > 0) $ tellLn $ styled Style.green $ show passed  <> " passing"
      when (pending > 0) $ tellLn $ styled Style.cyan $ show pending <> " pending"
      when (failed  > 0) $ tellLn $ styled Style.red $ show failed  <> " failed"
  tellLn ""
  printFailures xs

printFailures
  :: forall m
   . MonadWriter String m
  => Array (Tree Name Void Result)
  -> m Unit
printFailures xs' = evalStateT (go $ annotatedWithPaths xs') 0
  where
    go :: Array (Tree TestLocator Void Result) -> StateT Int m Unit
    go = traverse_ case _ of
      S.Node (Left _) xs -> go xs
      S.Node (Right v) _ -> absurd v
      S.Leaf (path /\ n) (Just (Failure err)) -> do
        i <- State.modify $ add 1
        let label = intercalate " " (parentSuiteName path <> [n])
        tellLn $ show i <> ") " <> label
        tellLn $ styled Style.red $ Style.indent 2 <> Error.message err
      S.Leaf _ _ -> pure unit

-- | Monadic left scan with state.
-- | TODO: Is this already included in purescript-pipes somehow, or should be?
scanWithStateM
  :: forall a x m r
   . Monad m
  => (x -> a -> m x) -> m x -> Pipe a a m r
scanWithStateM step begin = do
  x <- lift begin
  go x
  where
    go x = do
        a <- await
        yield a
        x' <- lift (step x a)
        go $ x'

-- | A default reporter implementation that can be used as a base to build
-- | other reporters on top of.
defaultReporter
  :: forall s
   . s
  -> (Event -> StateT s (Writer String) Unit)
  -> Reporter
defaultReporter initialState onEvent = pure initialState # scanWithStateM \s e ->
  let Tuple res log = runWriter $ execStateT (onEvent e) s
  in liftEffect $ Console.write log $> res


data RunningItem
  = RunningTest (Maybe Result)
  | RunningPending
  | RunningSuite Boolean

derive instance Generic RunningItem _
instance Show RunningItem where show = genericShow

defaultUpdate
  :: forall s
  . { getRunningItems :: s -> Map TestLocator RunningItem
    , putRunningItems :: Map TestLocator RunningItem -> s -> s
    , printFinishedItem :: TestLocator -> RunningItem -> StateT s (Writer String) Unit
    , update :: Event -> StateT s (Writer String) Unit
    }
  -> Event
  -> StateT s (Writer String) Unit
defaultUpdate opts e = do
  baseUpdate e
  opts.update e
  where
    baseUpdate = case _ of
      Event.Suite Event.Sequential _ ->
        pure unit
      Event.Suite Event.Parallel loc -> do
        modifyRunningItems $ Map.insert loc $ RunningSuite false
      Event.SuiteEnd loc -> do
        modifyRunningItems $ flip Map.update loc case _ of
          RunningSuite _ -> Just $ RunningSuite true
          _ -> Nothing
      Event.Test Event.Sequential _ -> do
        pure unit
      Event.Test Event.Parallel loc -> do
        modifyRunningItems $ Map.insert loc $ RunningTest Nothing
      Event.TestEnd loc res -> do
        runningItem <- gets opts.getRunningItems
        case Map.lookup loc runningItem of
          Just (RunningTest _) ->
            modifyRunningItems $ Map.insert loc $ RunningTest $ Just res
          _ ->
            pure unit
      Event.Pending loc -> do
        runningItem <- gets opts.getRunningItems
        unless (Map.isEmpty runningItem) do
          modifyRunningItems $ Map.insert loc RunningPending
      Event.End _ -> pure unit
      Event.Start _ -> pure unit

    modifyRunningItems f = do
      s <- get
      let
        nextRunningItems = f $ opts.getRunningItems s
        allFinished = all runningItemIsFinished nextRunningItems
      put $ opts.putRunningItems (if allFinished then Map.empty else nextRunningItems) s

      when allFinished do
        for_ (Map.toUnfoldable nextRunningItems :: Array _) $ uncurry opts.printFinishedItem
      where
        runningItemIsFinished = case _ of
          RunningPending -> true
          RunningTest res -> isJust res
          RunningSuite finished -> finished
