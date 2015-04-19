module Main where

import Test.Spec
import Test.Spec.Runner
import Test.Spec.Assertions

main = suite do
  describe "Math" do
    describe "Addition" do
        it "does addition" do
          (1 + 1) `shouldEqual` 2
        it "fails as well" do
          (1 + 1) `shouldEqual` 3
    describe "Multiplication" do
      pending "will do multiplication in the future"
