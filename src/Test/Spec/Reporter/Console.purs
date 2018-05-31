module Test.Spec.Reporter.Console (consoleReporter) where

import Prelude
import Test.Spec.Color as Color
import Test.Spec.Runner.Event as Event
import Test.Spec.Summary as Summary
import Effect (Effect)
import Effect.Console (log)
import Data.Array (init)
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe)
import Test.Spec.Color (colored)
import Test.Spec.Console (withAttrs)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Runner (Reporter)
import Test.Spec.Summary (Summary(..))

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

consoleReporter :: Reporter
consoleReporter = defaultReporter initialState update
  where
  update s = case _ of
    Event.Suite name -> pure (pushCrumb name s)
    Event.SuiteEnd -> pure (popCrumb s)
    Event.Pass name _ _ -> flushCrumbs do
      log $ "  " <> (colored Color.Checkmark "✓︎" <> " " <> colored Color.Pass name)
    Event.Pending name -> flushCrumbs do
      log $ "  " <> (colored Color.Pending $ "~ " <> name)
    Event.Fail name msg _ -> flushCrumbs do
      log $ "  " <> (colored Color.Fail $ "✗ " <> name <> ":")
      log ""
      log $ colored Color.Fail $ "  " <> msg
    Event.End results -> s <$ printSummary results
    _ -> pure s
      where
      flushCrumbs action =
        if not s.crumbsChanged
           then s <$ action
           else s { crumbsChanged = false, hasEmitted = true } <$ do
            when s.hasEmitted $ log ""
            withAttrs [1, 35] $ log $ intercalate " » " s.crumbs
            action

      printSummary = Summary.summarize >>> \(Count passed failed pending) -> do
        log ""
        withAttrs [1] $ log "Summary"
        printPassedFailed passed failed
        printPending pending
        log ""


pluralize :: String -> Int -> String
pluralize s 1 = s
pluralize s _ = s <> "s"

printPassedFailed :: Int -> Int -> Effect Unit
printPassedFailed p f = do
  let total = p + f
      testStr = pluralize "test" total
      amount = show p <> "/" <> (show total) <> " " <> testStr <> " passed"
      attrs = if f > 0 then [31] else [32]
  withAttrs attrs $ log amount

printPending :: Int -> Effect Unit
printPending p
  | p > 0     = withAttrs [33] $ log (show p <> " " <> pluralize "test" p <> " pending")
  | otherwise = pure unit
