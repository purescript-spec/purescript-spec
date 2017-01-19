module Test.Spec.Reporter.Base
       ( defaultUpdate
       , defaultSummary
       , defaultReporter
       ) where

import Prelude
import Control.Monad.Eff.Exception as Error
import Control.Monad.State as State
import Data.Array as Array
import Data.String as String
import Test.Spec as S
import Test.Spec.Color as Color
import Test.Spec.Summary as Summary
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array ((:), reverse)
import Data.Foldable (intercalate)
import Data.Traversable (for_)
import Pipes (await, yield)
import Pipes.Core (Pipe)
import Test.Spec (Group, Result(..))
import Test.Spec.Color (colored)
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Event)
import Test.Spec.Summary (Summary(..))

-- TODO: move this somewhere central
indent :: Int -> String
indent i = String.fromCharArray $ Array.replicate i ' '

defaultUpdate :: forall s e. s -> Event -> Eff e s
defaultUpdate s _ = pure s

defaultSummary :: forall s e. s -> Array (Group Result) -> Eff (console :: CONSOLE | e) Unit
defaultSummary _ xs = do
  case Summary.summarize xs of
    (Count passed failed pending) -> do
      when (passed  > 0) $ log $ colored Color.Green   $ show passed  <> " passing"
      when (pending > 0) $ log $ colored Color.Pending $ show pending <> " pending"
      when (failed  > 0) $ log $ colored Color.Fail    $ show failed  <> " failed"
  log ""
  printFailures xs


printFailures
  :: forall e
   . Array (Group Result)
  -> Eff (console :: CONSOLE | e) Unit
printFailures xs = void $ evalStateT (go [] xs) 0
  where
    go
      :: Array String
      -> Array (Group Result)
      -> StateT Int (Eff (console :: CONSOLE | e)) Unit
    go crumbs groups =
      for_ groups case _ of
        S.Describe _ n xs -> go (n:crumbs) xs
        S.It _ n (Failure err) ->
          let label = intercalate " " (reverse $ n:crumbs)
            in do
                State.modify (_ + 1)
                i <- State.get
                lift $ log $ show i <> ") " <> label
                lift $ log $ colored Color.ErrorMessage $ indent 2 <> Error.message err
        _ -> pure unit

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
  :: ∀ s e.
     s
  -> (s -> Event -> Eff e s)
  -> (s -> Array (Group Result) -> Eff e Unit)
  -> Reporter e
defaultReporter initialState onEvent onSummary =
  scanWithStateM dispatch (pure initialState)
  where
    dispatch s e = liftEff (onEvent s e)
