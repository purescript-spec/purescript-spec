module Test.Spec.Reporter.Tap (tapReporter) where

import Prelude
import Data.String.Regex as Regex
import Test.Spec.Runner.Event as Event
import Test.Spec.Summary as Summary
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(Pattern), joinWith, split)
import Data.String.Regex (regex)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Runner (Reporter)
import Test.Spec.Summary (Summary(..))

type TapReporterState = Int

tapReporter :: ∀ e. Reporter (console :: CONSOLE | e)
tapReporter =
 defaultReporter 1 update
 where
  update n = case _ of
    Event.Start nTests -> n <$ (log $ "1.." <> show nTests)
    Event.TestEnd -> pure (n + 1)
    Event.Pending name -> n <$ log do
      "ok " <> show n <> " " <> (escTitle name) <> " # SKIP -"
    Event.Pass name _ _ -> n <$ log do
      "ok " <> show n <> " " <> (escTitle name)
    Event.Fail name msg mStack -> n <$ do
      log $ "not ok " <> show n <> " " <> (escTitle name)
      log $ escMsg msg
      case mStack of
        Nothing -> pure unit
        Just s  -> log $ joinWith "\n" (append "    " <$> split (Pattern "\n") s)
    Event.End results -> do
      case Summary.summarize results of
        (Count passed failed pending) -> do
          log $ "# tests " <> show (failed + passed + pending)
          log $ "# pass "  <> show (passed + pending)
          log $ "# fail "  <> show failed
      pure n

    _ -> pure n

-- create a TAP-safe title
escMsg :: String -> String
escMsg =
  let rex = unsafePartial $ fromRight $ regex "^" $ Regex.parseFlags "gm"
    in Regex.replace rex "  "

-- create a TAP-safe error msg
escTitle :: String -> String
escTitle =
  let rex = unsafePartial $ fromRight $ regex "#" $ Regex.parseFlags "g"
    in Regex.replace rex ""
