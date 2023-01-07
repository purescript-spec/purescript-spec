module Test.Spec.Reporter.TeamCitySpec (teamcitySpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.TeamCity as TC

teamcitySpec :: Spec Unit
teamcitySpec = describe "Team City Reporter" do
  it "can print Team City Service Messages" do
    TC.teamcity "testStarted" { name: "test.name", nodeId: "null", parentNodeId: Nothing }
      # shouldEqual "##teamcity[testStarted name='test.name' nodeId='null' parentNodeId='0']"
  it "escapes property values" do
    TC.teamcity "testStarted" { name: "|test\n\r[nam'e]", nodeId: "null", parentNodeId: Nothing }
      # shouldEqual "##teamcity[testStarted name='||test|n|r|[nam|'e|]' nodeId='null' parentNodeId='0']"

