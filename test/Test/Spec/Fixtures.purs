module Test.Spec.Fixtures where

import Prelude

import Data.Identity (Identity)
import Test.Spec (SpecT, describe, describeOnly, it, itOnly)

type Spec' a = SpecT Identity Unit Identity a

successTest :: Spec' Unit
successTest =
  describe "a" do
    describe "b" do
      it "works" $ pure unit

sharedDescribeTest :: Spec' Unit
sharedDescribeTest =
  describe "a" do
    describe "b" do
      it "works" $ pure unit
    describe "c" do
      it "also works" $ pure unit

duplicatedDescribeTest :: Spec' Unit
duplicatedDescribeTest =
  describe "a" do
    describe "b" do
      describe "c" do
        it "first" $ pure unit
    describe "b" do
      describe "c" do
        it "second" $ pure unit

describeOnlyTest :: Spec' Unit
describeOnlyTest =
  describeOnly "a" do
    describe "b" do
      it "works" $ pure unit
    describe "c" do
      it "also works" $ pure unit

describeOnlyNestedTest :: Spec' Unit
describeOnlyNestedTest =
  describe "a" do
    describeOnly "b" do
      it "works" $ pure unit
    describe "c" do
      it "also works" $ pure unit

itOnlyTest :: Spec' Unit
itOnlyTest =
  describe "a" do
    describe "b" do
      itOnly "works" $ pure unit
    describe "c" do
      it "also works" $ pure unit
