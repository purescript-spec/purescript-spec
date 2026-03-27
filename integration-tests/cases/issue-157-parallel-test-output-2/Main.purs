module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (Spec, describe, it, parallel)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] spec

-- When the test tree is parallel, and when it's arranged _just_so_, the very
-- last test (in this case - test 'D') used to get swallowed by the
-- `mergeProducers` function, which used to call `AVar.kill` at the end, thus
-- destroying the last value in the var.
spec :: Spec Unit
spec = parallel $
  describe "A" do
    it "B" do
      pure unit

    describe "C" do
      it "D" do
        pure unit

    it "E" do
      pure unit
