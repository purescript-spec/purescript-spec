module Main where

import Test.Spec
import Test.Spec.Runner
import Test.Spec.Assertions

main = suite $
  describe "Math" do
    it "does addition" do
      (1 + 1) `shouldEqual` 2
    it "fails as well" do
      (1 + 1) `shouldEqual` 3
    pending "will do stuff in the future"
