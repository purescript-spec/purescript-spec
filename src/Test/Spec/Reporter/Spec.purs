module Test.Spec.Reporter.Spec (specReporter) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error(), message)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (StateT, execStateT, evalStateT)
import Control.Monad.State as State
import Data.Foldable (intercalate, traverse_)
import Data.Traversable (for)
import Data.Map (toList)
import Data.Array ((:), reverse)
import Data.Array as Array
import Data.String as String
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Test.Spec (Group, Result(..))
import Test.Spec as S
import Test.Spec.Console (withAttrs)
import Test.Spec.Reporter (collapseAll, EntryPath, Entry(..), Reporter)
import Test.Spec.Summary (Summary(..), summarize)

type NamedFailure = Tuple String Error
type ReporterState = { failures :: Array NamedFailure }

-- TODO: move these somewhere central (Test.Spec.Console?)
red   = withAttrs [31]
green = withAttrs [32]
blue  = withAttrs [33]

specReporter :: forall e. Reporter (console :: CONSOLE | e)
specReporter groups = do
  { failures } <- flip execStateT { failures: [] } $ printGroups 0 [] groups
  log ""
  printSummary groups
  log ""
  printFailures $ reverse failures

  where
  printGroups :: forall r. Int
                  -> Array String
                  -> Array (Group Result)
                  -> StateT ReporterState (Eff (console :: CONSOLE | r)) Unit
  printGroups i crumbs groups = do
    void $ for groups case _ of
      S.Describe _ n xs -> do
        logPlain n
        printGroups (i + 1) (n : crumbs) xs
      S.Pending n -> logBlue $ "- " <> n
      S.It _ n result ->
        case result of
          Success -> logGreen $ "✓︎ " <> n
          Failure err ->
            let label = intercalate " " (reverse $ n : crumbs)
             in do
              State.modify \s -> s { failures = (label /\ err) : s.failures }
              nFailures <- Array.length <<< _.failures <$> State.get
              logRed $ show nFailures <> ") " <> n

    where
    _log msg = log $ indent i <> msg
    logRed   = lift <<< red   <<< _log
    logGreen = lift <<< green <<< _log
    logBlue  = lift <<< red   <<< _log
    logPlain = lift           <<< _log

  printFailures :: forall r. Array NamedFailure
                  -> Eff (console :: CONSOLE | r) Unit
  printFailures failures = void $ flip evalStateT 1 do
    for failures \(n /\ err) -> do
      i <- State.get
      lift $ log $ show i <> ") " <> n
      lift $ red $ log $ indent 2 <> message err
      State.modify $ const $ i + 1

  indent :: Int -> String
  indent i = String.fromCharArray (Array.replicate i ' ')

  printSummary :: forall r. Array (Group Result)
                  -> Eff (console :: CONSOLE | r) Unit
  printSummary = summarize >>> \(Count passed failed pending) -> do
    when (passed  > 0) $ green $ log $ show passed  <> " passing"
    when (pending > 0) $ blue  $ log $ show pending <> " pending"
    when (failed  > 0) $ red   $ log $ show failed  <> " failed"
