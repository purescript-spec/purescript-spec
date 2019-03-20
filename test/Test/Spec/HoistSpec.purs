module Test.Spec.HoistSpec where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array.NonEmpty as NAE
import Data.Identity (Identity)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (intercalate)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Test.Spec (ComputationType(..), Spec, SpecT, describe, hoistSpec, it, parallel)

type Spec' t a = SpecT t Unit Identity a

hoistSpecSpec :: Spec Unit
hoistSpecSpec = describe "hoist" do
  describe "normal" $ delaySpecExample {log, delay}
  describe "reader" $ hoistSpecSpecReaderT


hoistSpecSpecReaderT :: Spec Unit
hoistSpecSpecReaderT = go $ parallel do
  delaySpecExample
    { log: \s -> ask >>= \logger -> liftAff $ logger s
    , delay: \ms -> liftAff $ delay ms
    }
  where
    go :: Spec' (ReaderT (String -> Aff Unit) Aff) ~> Spec
    go = hoistSpec identity \cType m ->
      let
        prefix = case cType of
          CleanUpWithContext n -> intercalate " > " n <> " (afterAll) "
          TestWithName n -> intercalate " > " $ NAE.toArray n
      in runReaderT m \logMsg -> log $ prefix  <> "| " <> logMsg

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
    opts.delay $ Milliseconds $ 300.0 * 1.0
    opts.log "done 1"
  describe "some" do
    it "proc 2" do
      opts.log "start 2"
      opts.delay $ Milliseconds $ 300.0 * 2.0
      opts.log "done 2"
    it "proc 3" do
      opts.log "start 3"
      opts.delay $ Milliseconds $ 300.0 * 3.0
      opts.log "done 3"
    describe "nesting" do
      it "proc 4" do
        opts.log "start 4"
        opts.delay $ Milliseconds $ 300.0 * 4.0
        opts.log "done 4"
    describe "nesting" do
      it "proc 5" do
        opts.log "start 5"
        opts.delay $ Milliseconds $ 300.0 * 5.0
        opts.log "done 5"
      it "proc 6" do
        opts.log "start 6"
        opts.delay $ Milliseconds $ 300.0 * 6.0
        opts.log "done 6"
