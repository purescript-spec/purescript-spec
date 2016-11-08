module Test.Spec.Reporter.Spec (specReporter) where

import Prelude

import Data.String as String
import Data.Array as Array
import Data.Array ((:), reverse)
import Data.Foldable (intercalate)
import Data.Traversable (for_)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Exception as Error

import Test.Spec as S
import Test.Spec.Reporter.Base (BaseReporter, reporter)
import Test.Spec (Group, Result(..))
import Test.Spec.Console (withAttrs)
import Test.Spec.Runner.Event as Event
import Test.Spec.Summary as Summary
import Test.Spec.Summary (Summary(..))

-- TODO: move these somewhere central (Test.Spec.Console?)
red   = withAttrs [31]
green = withAttrs [32]
blue  = withAttrs [36]

type SpecReporterStateObj = {
  indent :: Int
, numFailures :: Int
}

specReporter :: ∀ e. BaseReporter SpecReporterStateObj (Eff (console :: CONSOLE | e))
specReporter = reporter { indent: 0, numFailures: 0 } update summarize where
  update s = case _ of
    Event.Start -> s <$ do
      log ""
    Event.Suite name ->
      let s' = s { indent = s.indent + 1 }
       in s' <$ _log name
    Event.SuiteEnd ->
      let s' = s { indent = s.indent - 1 }
       in s' <$ when (s.indent == 1) (log "")
    Event.Pending name -> s <$ do
      logBlue $ "- " <> name
    Event.Pass name -> s <$ do
      logGreen $ "✓︎ " <> name
    Event.Fail name _ ->
      let s' = s { numFailures = s.numFailures + 1 }
       in s' <$ do
          logRed $ show s'.numFailures <> ") " <> name
    _ -> pure s

    where
    _log msg = log $ indent s.indent <> msg
    logRed   = red   <<< _log
    logGreen = green <<< _log
    logBlue  = blue  <<< _log

  summarize _ xs = do
    case Summary.summarize xs of
      (Count passed failed pending) -> do
        when (passed  > 0) $ green $ log $ show passed  <> " passing"
        when (pending > 0) $ blue  $ log $ show pending <> " pending"
        when (failed  > 0) $ red   $ log $ show failed  <> " failed"
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
                lift $ red $ log $ indent 2 <> Error.message err
        _ -> pure unit

  -- TODO: move this somewhere central
  indent i = String.fromCharArray $ Array.replicate i ' '

