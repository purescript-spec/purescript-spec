module Test.Spec.Reporter.TCSpec (tcSpec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Tuple.Nested ((/\))
import Test.Spec.Reporter.TC as TC

tcSpec :: Spec Unit
tcSpec = describe "Team City Preorter" do
  it "can print Team City Service Messages" do
    TC.teamcity "testStarted" ["name" /\ "test.name"]
      # shouldEqual "##teamcity[testStarted name='test.name']"
  it "escapes property values" do
    TC.teamcity "testStarted" ["name" /\ "|test\n\r[nam'e]"]
        # shouldEqual "##teamcity[testStarted name='||test|n|r|[nam|'e|]']"

