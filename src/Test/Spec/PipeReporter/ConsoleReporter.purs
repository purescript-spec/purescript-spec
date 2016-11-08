module Test.Spec.PipeReporter.ConsoleReporter (consoleReporter) where

import Prelude
import Data.Array (init)
import Data.Maybe (fromMaybe)
import Data.Foldable (intercalate)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Spec.Summary as Summary
import Test.Spec.Summary (Summary(..))
import Test.Spec (Group, Result(..))
import Test.Spec.Console (withAttrs)
import Test.Spec.Runner.Event as Event
import Test.Spec.PipeReporter.BaseReporter (BaseReporter, reporter)

-- TODO: move these somewhere central (Test.Spec.Console?)
red   = withAttrs [31]
green = withAttrs [32]
blue  = withAttrs [36]

type ConsoleReporterStateObj = {
  crumbs :: Array String
, crumbsChanged :: Boolean
, hasEmitted :: Boolean
}

initialState :: ConsoleReporterStateObj
initialState = {
  crumbs: []
, crumbsChanged: false
, hasEmitted: false
}

pushCrumb :: String -> ConsoleReporterStateObj -> ConsoleReporterStateObj
pushCrumb c s = s {
  crumbs = s.crumbs <> [c]
, crumbsChanged = true
}

popCrumb :: ConsoleReporterStateObj -> ConsoleReporterStateObj
popCrumb s = s {
  crumbs = fromMaybe [] $ init s.crumbs
, crumbsChanged = true
}

consoleReporter :: ∀ e. BaseReporter ConsoleReporterStateObj (Eff (console :: CONSOLE | e))
consoleReporter = reporter initialState update summarize where
  update s = case _ of
    Event.Suite name -> pure (pushCrumb name s)
    Event.SuiteEnd -> pure (popCrumb s)
    Event.Pass name -> flushCrumbs do
      green $ log $  "✓︎ " <> name
    Event.Pending name -> flushCrumbs do
      blue $ log $  "~ " <> name
    Event.Fail name msg -> flushCrumbs do
      red $ log $ "✗ " <> name <> ":"
      log ""
      red $ log $ "  " <> msg
    _ -> pure s
      where
      flushCrumbs action =
        if not s.crumbsChanged
           then s <$ action
           else s { crumbsChanged = false, hasEmitted = true } <$ do
            when s.hasEmitted $ log ""
            withAttrs [1, 35] $ log $ intercalate " » " s.crumbs
            action
  summarize _ = printSummary

pluralize :: String -> Int -> String
pluralize s 1 = s
pluralize s _ = s <> "s"

printPassedFailed :: forall r. Int -> Int -> Eff (console :: CONSOLE | r) Unit
printPassedFailed p f = do
  let total = p + f
      testStr = pluralize "test" total
      amount = show p <> "/" <> (show total) <> " " <> testStr <> " passed"
      attrs = if f > 0 then [31] else [32]
  withAttrs attrs $ log amount

printPending :: forall r. Int -> Eff (console :: CONSOLE | r) Unit
printPending p
  | p > 0     = withAttrs [33] $ log (show p <> " " <> pluralize "test" p <> " pending")
  | otherwise = pure unit

printSummary :: forall r. Array (Group Result) -> Eff (console :: CONSOLE | r) Unit
printSummary = Summary.summarize >>> \(Count passed failed pending) -> do
  log ""
  withAttrs [1] $ log "Summary"
  printPassedFailed passed failed
  printPending pending
  log ""
