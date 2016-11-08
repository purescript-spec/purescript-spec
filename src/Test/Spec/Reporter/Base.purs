module Test.Spec.Reporter.Base (
  update,
  summarize,
  reporter,
  defaultReporter,
  onSummarize,
  onUpdate,
  BaseReporter
  ) where

import Prelude

import Data.String as   String
import Data.Array as    Array
import Data.Traversable (for_)
import Data.Array       ((:), reverse)
import Data.Foldable    (intercalate)

import Control.Monad.Eff              (Eff)
import Control.Monad.Eff.Console      (CONSOLE, log)
import Control.Monad.State            (StateT, evalStateT)
import Control.Monad.State as         State
import Control.Monad.Trans.Class      (lift)
import Control.Monad.Eff.Exception as Error

import Test.Spec as           S
import Test.Spec              (Group(), Result(..))
import Test.Spec.Color as     Color
import Test.Spec.Color        (colored)
import Test.Spec.Runner.Event (Event)
import Test.Spec.Summary      as Summary
import Test.Spec.Summary      (Summary(..))
import Test.Spec.Console      (withAttrs)

type Update s m r = s -> Event -> m r
type Summarize s m = s -> Array (Group Result) -> m Unit

-- A reporter is responsible for reporting test results in some monad `m`.
-- Reporters keep a state `s` that is updated with each application of the
-- `update` function. Finally a reporter may be asked to report a summary of
-- the test results, given a set of test results as well as the latest `s` it
-- built.
newtype BaseReporter s m = BaseReporter {
  state     :: s
, update    :: Update s m (BaseReporter s m)
, summarize :: Summarize s m
}

-- Update the reporter, given events emitted by the runner.
-- It returns a new reporter, which then in turn can be updated to
-- effectively fold over the incoming events.
update :: ∀ s m. Event -> BaseReporter s m -> m (BaseReporter s m)
update e (BaseReporter { state: s, update: f }) = f s e

-- Handle summarizing the results
-- The effect runs in `m` and returns Unit, so is only useful for the
-- side-effects it causes.
summarize :: ∀ s m. Array (Group Result) -> BaseReporter s m -> m Unit
summarize xs (BaseReporter { state, summarize: f }) = f state xs

-- set the update handler for a reporter
onUpdate
  :: ∀ s m
   . (Monad m)
  => Update s m s
  -> BaseReporter s m
  -> BaseReporter s m
onUpdate update (BaseReporter s) = reporter s.state update s.summarize

-- set the summary handler for a reporter
onSummarize
  :: ∀ s m
   . Summarize s m
  -> BaseReporter s m
  -> BaseReporter s m
onSummarize summarize (BaseReporter s) = BaseReporter $ s { summarize = summarize }

-- implement a fresh reporter
reporter
  :: ∀ s m
   . (Monad m)
  => s                -- the initial state of the reporter
  -> Update s m s     -- the update function
  -> Summarize s m    -- 
  -> BaseReporter s m
reporter state update summarize = BaseReporter { state, update: go, summarize }
  where
  go s e = do
    s' <- update s e
    pure $ BaseReporter { state: s', update: go, summarize }

-- | A default reporter implementation that can be used as a base to build
-- | other reporters on top of.
defaultReporter :: ∀ s e. s -> BaseReporter s (Eff (console :: CONSOLE | e))
defaultReporter s = reporter s defaultUpdate defaultSummary
  where
  defaultUpdate s _ = pure s
  defaultSummary  _ xs = do
    case Summary.summarize xs of
      (Count passed failed pending) -> do
        when (passed  > 0) $ log $ colored Color.Green   $ show passed  <> " passing"
        when (pending > 0) $ log $ colored Color.Pending $ show pending <> " pending"
        when (failed  > 0) $ log $ colored Color.Fail    $ show failed  <> " failed"
    log ""
    printFailures xs

  printFailures
    :: Array (Group Result)
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

  -- TODO: move this somewhere central
  indent i = String.fromCharArray $ Array.replicate i ' '

