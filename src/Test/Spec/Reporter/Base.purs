module Test.Spec.Reporter.Base
       ( defaultUpdate
       , defaultSummary
       , defaultReporter
       ) where

import Prelude

import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Array ((:), reverse)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception as Error
import Pipes (await, yield)
import Pipes.Core (Pipe)
import Test.Spec (Result, Tree)
import Test.Spec as S
import Test.Spec.Color (colored)
import Test.Spec.Color as Color
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Event)
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary

-- TODO: move this somewhere central
indent :: Int -> String
indent i = CodeUnits.fromCharArray $ Array.replicate i ' '

defaultUpdate :: forall s. s -> Event -> Effect s
defaultUpdate s _ = pure s

defaultSummary :: Array (Tree Void Result) -> Effect Unit
defaultSummary xs = do
  case Summary.summarize xs of
    (Count {passed, failed, pending}) -> do
      when (passed  > 0) $ log $ colored Color.Green   $ show passed  <> " passing"
      when (pending > 0) $ log $ colored Color.Pending $ show pending <> " pending"
      when (failed  > 0) $ log $ colored Color.Fail    $ show failed  <> " failed"
  log ""
  printFailures xs


printFailures
  :: Array (Tree Void Result)
  -> Effect Unit
printFailures xs = void $ evalStateT (go [] xs) 0
  where
    go
      :: Array String
      -> Array (Tree Void Result)
      -> StateT Int Effect Unit
    go crumbs groups =
      for_ groups case _ of
        S.Node (Left n) xs' -> go (n:crumbs) xs'
        S.Node (Right _) xs' -> go crumbs xs'
        S.Leaf n (Just (S.Failure err)) ->
          let label = intercalate " " (reverse $ n:crumbs)
            in do
                _ <- State.modify (_ + 1)
                i <- State.get
                lift $ log $ show i <> ") " <> label
                lift $ log $ colored Color.ErrorMessage $ indent 2 <> Error.message err
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
  -> (s -> Event -> Effect s)
  -> Reporter
defaultReporter initialState onEvent = do
  scanWithStateM dispatch (pure initialState)
  where
    dispatch s e = liftEffect(onEvent s e)
