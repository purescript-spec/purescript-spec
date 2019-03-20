module Test.Spec.HookSpec where

import Prelude

import Test.Spec (Spec, after, aroundWith, before, beforeWith, describe, it)
import Test.Spec.Assertions (shouldEqual)

hookSpec :: Spec Unit
hookSpec = do
  describe "Test" do
    describe "Spec" do
      describe "hooks" do
        it "a regular test case" do
          1 `shouldEqual` 1
        before (pure 1) $ after (\a -> a `shouldEqual` 1) do
          it "before & after usage" \num -> do
            num `shouldEqual` 1
          beforeWith (\num -> num `shouldEqual` 1 *> pure true) do
            it "beforeWith usage" \bool -> do
              bool `shouldEqual` true
            aroundWith (\computation bool -> bool `shouldEqual` true *> pure "fiz" >>= computation <* pure unit) do
              it "aroundWith usage" \str -> do
                str `shouldEqual` "fiz"
          beforeWith (\num -> num `shouldEqual` 1 *> pure (show num)) do
            it "beforeWith" \str -> do
              str `shouldEqual` "1"
