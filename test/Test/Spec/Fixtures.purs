module Test.Spec.Fixtures where

import Prelude

import Control.Monad.Aff         (Aff())
import Control.Monad.State.Trans (StateT())

import Test.Spec            (Group(), describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

successTest :: forall eff. StateT (Array Group) (Aff eff) Unit
successTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1

sharedDescribeTest :: forall eff. StateT (Array Group) (Aff eff) Unit
sharedDescribeTest =
  describe "a" do
    describe "b" do
      it "works" do
        1 `shouldEqual` 1
    describe "c" do
      it "also works" do
        1 `shouldEqual` 1

failureTest :: forall eff. StateT (Array Group) (Aff eff) Unit
failureTest = it "fails" $ 1 `shouldEqual` 2

pendingTest :: forall eff. StateT (Array Group) (Aff eff) Unit
pendingTest = pending "is not written yet"
