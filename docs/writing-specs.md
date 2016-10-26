# Writing Specs

The basic building block of spec writing is `it`, which creates a spec with a
*spec body*. In the following example we use `pure unit` as a body, which does
nothing.

```purescript
it "does nothing" $ pure unit
```

A more interesting test would assert something. Let's check that addition
works.

```purescript
it "adds 1 and 1" do
  1 + 1 `shouldEqual` 2
```

Specs can also be *pending*, which means that they are not testing anything
yet - they are like placeholders. We use `pending` to write a pending spec.

```purescript
pending "calculates the answer to Life, the Universe and Everything"
```

Pending specs can also contain a spec body. These are used to give a hint what
the spec should assert in the future, without actually doing the assert. Use
`pending'` (note the `'` at the end) to create such a pending spec.

```purescript
pending' "calculates the answer to Life, the Universe and Everything" do
  answerTo theUltimateQuestion `shouldBe` 42
```

To group multiple specs in a larger, logically related group of specs, we use
`describe`.

```purescript
describe "MyModule" do
  it "..." do
    ...
  it "..." do
    ...
  it "..." do
    ...
```

## Full Example

Let's look at an example of a complete spec program, with the needed imports
and a proper `main` function. The specs shown in the [header
image](#header-image) looks like this:

```purescript
module Main where

import Prelude

import Control.Monad.Aff          (later')
import Test.Spec                  (describe, it)
import Test.Spec.Runner           (run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
      pending "feature complete"
    describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports async specs" do
        res <- later' 100 $ pure "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.10.1 compatible" $ pure unit
```

## Asynchronous Specs

Specs can be async as the spec body type of `it` and `pending'` is
[forall e. Aff e Unit](https://github.com/slamdata/purescript-aff).


## Combining Specs

You can split test specs into multiple files and combine
them using `do` expressions.

```purescript
baseSpecs = do
  mathSpec
  stringsSpec
  arraySpec
  ...
```

This is often used to combine all specs into a single spec that can be passed
to the test runner.

## QuickCheck

You can use [QuickCheck](https://github.com/purescript/purescript-quickcheck)
together with the [`purescript-spec-quickcheck`](https://github.com/owickstrom/purescript-spec-quickcheck)
adapter to get nice output formatting for QuickCheck tests.
