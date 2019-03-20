module Test.Spec.ParallelSpec where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Test.Spec (Spec, describe, it, parallel, pending, sequential)
-- import Test.Spec.Assertions (shouldEqual)

parallelSpec :: Spec Unit
parallelSpec = do
  describe "g" do
    it "g.1" $ delay $ Milliseconds 500.0
    it "g.2" $ delay $ Milliseconds 500.0
    pending "g.3"
  describe "p" do
    describe "pp" do
      describe "ppp" do
        pending "ppp.1"
  describe "a" $ parallel do
    -- it "a.err" $ delay (Milliseconds 300.0) *> 1 `shouldEqual` 2
    it "a.1" $ delay $ Milliseconds 1500.0
    it "a.2" $ delay $ Milliseconds 500.0
    describe "d" $ sequential do
      it "d.1" $ delay $ Milliseconds 500.0
      it "d.2" $ delay $ Milliseconds 500.0
    describe "z" do
      it "z.1" $ delay $ Milliseconds 700.0
      it "z.2" $ delay $ Milliseconds 900.0
      pending "z.3"
      -- it "z.err" $ delay (Milliseconds 300.0) *> 1 `shouldEqual` 2
    describe "j" do
      it "j.1" $ delay $ Milliseconds 1000.0
      it "j.2" $ delay $ Milliseconds 400.0
  describe "k" do
    it "k.1" $ delay $ Milliseconds 500.0
    it "k.2" $ delay $ Milliseconds 500.0
