module Test.Spec.Reporter.Base
       ( defaultUpdate
       , defaultSummary
       , defaultReporter
       ) where

import Prelude

import Control.Monad.State (StateT, evalStateT, execStateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadWriter, Writer, runWriter)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate, traverse_)
import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Effect (Effect)
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
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary

-- TODO: move this somewhere central
indent :: Int -> String
indent i = CodeUnits.fromCharArray $ Array.replicate i ' '

defaultUpdate :: forall s. s -> Event -> Effect s
defaultUpdate s _ = pure s

defaultSummary :: forall m
   . MonadWriter String m
  => Array (Tree Void Result)
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
  => Array (Tree Void Result)
  -> m Unit
printFailures xs' = evalStateT (go xs') {i: 0, crumbs: Nil}
  where
    go :: Array (Tree Void Result) -> StateT { i :: Int, crumbs :: List String } m Unit
    go = traverse_ case _ of
      S.Node (Left n) xs -> do
        {crumbs} <- State.get
        State.modify_ _{crumbs = n : crumbs}
        go xs
        State.modify_ _{crumbs = crumbs}
      S.Node (Right _) xs -> go xs
      S.Leaf n (Just (Failure err)) -> do
        {i, crumbs} <- State.modify \s -> s{i = s.i +1}
        let label = intercalate " " (reverse $ n:crumbs)
        tellLn $ show i <> ") " <> label
        tellLn $ styled Style.red $ indent 2 <> Error.message err
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
        a  <- await
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
