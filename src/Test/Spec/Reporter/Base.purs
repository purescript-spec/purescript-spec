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

type Update c s m r = c -> s -> Event -> m r
type Summarize c s m = c -> s -> Array (Group Result) -> m Unit

-- A reporter is responsible for reporting test results in some monad `m`.
-- Reporters keep a state `s` that is updated with each application of the
-- `update` function. Finally a reporter may be asked to report a summary of
-- the test results, given a set of test results as well as the latest `s` it
-- built.
newtype BaseReporter c s m = BaseReporter {
  state     :: s
, config    :: c
, update    :: Update c s m (BaseReporter c s m)
, summarize :: Summarize c s m
}

-- Update the reporter, given events emitted by the runner.
-- It returns a new reporter, which then in turn can be updated to
-- effectively fold over the incoming events.
update :: ∀ c s m. Event -> BaseReporter c s m -> m (BaseReporter c s m)
update e (BaseReporter { state, config, update: f }) = f config state e

-- Handle summarizing the results
-- The effect runs in `m` and returns Unit, so is only useful for the
-- side-effects it causes.
summarize :: ∀ c s m. Array (Group Result) -> BaseReporter c s m -> m Unit
summarize xs (BaseReporter { config, state, summarize: f }) = f config state xs

-- set the update handler for a reporter
onUpdate
  :: ∀ s m c
   . (Monad m)
  => Update c s m s
  -> BaseReporter c s m
  -> BaseReporter c s m
onUpdate update' (BaseReporter s) = reporter s.config s.state update' s.summarize

-- set the summary handler for a reporter
onSummarize
  :: ∀ c s m
   . Summarize c s m
  -> BaseReporter c s m
  -> BaseReporter c s m
onSummarize summarize' (BaseReporter s) = BaseReporter $ s { summarize = summarize' }

-- implement a fresh reporter
reporter
  :: ∀ c s m
   . (Monad m)
  => c                -- the read-only config of the reporter
  -> s                -- the initial state of the reporter
  -> Update c s m s   -- the update function
  -> Summarize c s m  -- the summary reporting function
  -> BaseReporter c s m
reporter config state update' summarize' =
  BaseReporter { config
               , state
               , update: go
               , summarize: summarize'
               }
  where
  go c s e = do
    s' <- update' c s e
    pure $ BaseReporter { config, state: s', update: go, summarize: summarize' }

-- | A default reporter implementation that can be used as a base to build
-- | other reporters on top of.
defaultReporter :: ∀ c s e. c -> s -> BaseReporter c s (Eff (console :: CONSOLE | e))
defaultReporter c s = reporter c s defaultUpdate defaultSummary
  where
  defaultUpdate _ s _ = pure s
  defaultSummary _ _ xs = do
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
