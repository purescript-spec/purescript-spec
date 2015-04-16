module Main where

import Test.Spec
import Test.Spec.Runner
import Test.Spec.Assertions

main = suite $
  describe "Math" do
    it "works" do
      (1 + 1) `shouldEqual` 2
    it "does not work" do
      (1 + 1) `shouldNotEqual` 2
    pending "this is not used yet"
