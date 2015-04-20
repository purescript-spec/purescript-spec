module Main where

import Test.Spec.Runner
import Test.Spec.Assertions

additionSpec =
  describe "Addition" do
    it "does addition" do
      (1 + 1) `shouldEqual` 2
    it "fails as well" do
      (1 + 1) `shouldEqual` 3

main = suite $
  describe "Math" do
    additionSpec
    describe "Multiplication" do
      pending "will do multiplication in the future"
