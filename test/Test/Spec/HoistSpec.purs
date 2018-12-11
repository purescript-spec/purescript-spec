module Test.Spec.HoistSpec where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Semigroup.Foldable (intercalate)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Test.Spec (Spec, Spec', describe, hoistSpec, hoistSpec', it, parallel)

hoistSpecSpec :: Spec Unit
hoistSpecSpec = describe "hoist" do
  describe "normal" $ delaySpecExample {log, delay}
  describe "writer" $ hoistSpecSpecWriterT
  describe "reader" $ hoistSpecSpecReaderT

hoistSpecSpecWriterT :: Spec Unit
hoistSpecSpecWriterT = go $ parallel do
  delaySpecExample
    { log: \s -> tell [s]
    , delay: \ms -> liftAff $ delay ms
    }
  where
    go :: Spec' (WriterT (Array String) Aff) ~> Spec
    go = hoistSpec \m -> do
     Tuple res logMsgs <- runWriterT m
     for_ logMsgs log
     pure res

hoistSpecSpecReaderT :: Spec Unit
hoistSpecSpecReaderT = go $ parallel do
  delaySpecExample
    { log: \s -> ask >>= \logger -> liftAff $ logger s
    , delay: \ms -> liftAff $ delay ms
    }
  where
    go :: Spec' (ReaderT (String -> Aff Unit) Aff) ~> Spec
    go = hoistSpec' \{name} m -> runReaderT m \logMsg -> log $ intercalate " > " name <> "| " <>logMsg

delaySpecExample
  :: forall m
  . Monad m
  =>  { log :: String -> m Unit
      , delay :: Milliseconds -> m Unit
      }
  -> Spec' m Unit
delaySpecExample opts = describe "delay" do
  it "proc 1" do
    opts.log "start 1"
    opts.delay $ Milliseconds $ 500.0 + 300.0 * 1.0
    opts.log "done 1"
  it "proc 2" do
    opts.log "start 2"
    opts.delay $ Milliseconds $ 500.0 + 300.0 * 2.0
    opts.log "done 2"
  it "proc 3" do
    opts.log "start 3"
    opts.delay $ Milliseconds $ 500.0 + 300.0 * 3.0
    opts.log "done 3"
  it "proc 4" do
    opts.log "start 4"
    opts.delay $ Milliseconds $ 500.0 + 300.0 * 4.0
    opts.log "done 4"
  it "proc 5" do
    opts.log "start 5"
    opts.delay $ Milliseconds $ 500.0 + 300.0 * 5.0
    opts.log "done 5"
  it "proc 6" do
    opts.log "start 6"
    opts.delay $ Milliseconds $ 500.0 + 300.0 * 6.0
    opts.log "done 6"