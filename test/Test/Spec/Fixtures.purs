module Test.Spec.Fixtures where

import Prelude

import Data.Identity (Identity)
import Test.Spec (SpecM, describe, describeOnly, it, itOnly)

successTest :: SpecM Identity Identity Unit Unit
successTest =
  describe "a" do
    describe "b" do
      it "works" $ pure unit

sharedDescribeTest :: SpecM Identity Identity Unit Unit
sharedDescribeTest =
  describe "a" do
    describe "b" do
      it "works" $ pure unit
    describe "c" do
      it "also works" $ pure unit

duplicatedDescribeTest :: SpecM Identity Identity Unit Unit
duplicatedDescribeTest =
  describe "a" do
    describe "b" do
      describe "c" do
        it "first" $ pure unit
    describe "b" do
      describe "c" do
        it "second" $ pure unit

describeOnlyTest :: SpecM Identity Identity Unit Unit
describeOnlyTest =
  describeOnly "a" do
    describe "b" do
      it "works" $ pure unit
    describe "c" do
      it "also works" $ pure unit

describeOnlyNestedTest :: SpecM Identity Identity Unit Unit
describeOnlyNestedTest =
  describe "a" do
    describeOnly "b" do
      it "works" $ pure unit
    describe "c" do
      it "also works" $ pure unit

itOnlyTest :: SpecM Identity Identity Unit Unit
itOnlyTest =
  describe "a" do
    describe "b" do
      itOnly "works" $ pure unit
    describe "c" do
      it "also works" $ pure unit
