module Main where

import Test.Spec (describe, pending, it)
import Test.Spec.Node
import Test.Spec.Assertions
import Test.QuickCheck

additionSpec =
  describe "Addition" do
    it "does addition" do
      (1 + 1) `shouldEqual` 2
    it "fails as well" do
      (1 + 1) `shouldEqual` 3

main = runNode do
  describe "Math" do
    additionSpec
    describe "Multiplication" do
      pending "will do multiplication in the future"
  describe "Tools" $
    describe "QuickCheck" $
      it "works too" $
        quickCheck \n -> n + 1 > n
