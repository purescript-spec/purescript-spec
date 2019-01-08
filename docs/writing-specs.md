# Writing Specs

The basic building block of spec writing is `it`, which creates a spec with a
*spec body*. Spec bodies have the type `Aff Unit`, which is similar to the
`Effect` type, but with the addition of asynchronicity. When specs are run, they
are considered successful, or *passing*, if the Aff computation does not result
in an error. For more information, see [purescript-aff](https://github.com/slamdata/purescript-aff).

In the following example we use `pure unit` as a body, which does nothing. It
will not throw an error, and the spec will always pass.

```purescript
it "does nothing" $ pure unit
```

A more interesting test would assert something. Let's check that addition
works!

```purescript
it "adds 1 and 1" do
  1 + 1 `shouldEqual` 2
```

The `shouldEqual` function, here used as an infix operator, takes two values
and checks if they are equal. If not, it throws an error in the Aff monad,
causing the spec to fail.

Specs can also be *pending*, which means that they are not testing anything
yet - they are like placeholders. We use `pending` to write a pending spec.

```purescript
pending "calculates the answer to Life, the Universe and Everything"
```

Pending specs can also contain a spec body, just like with `it`. The difference
is that the body will be ignored. Pending spec bodies are used to give a hint
what the spec should assert in the future. Use `pending'` (note the `'` at the
end) to create a pending spec with a body.

```purescript
pending' "calculates the answer to Life, the Universe and Everything" do
  answerTo theUltimateQuestion `shouldBe` 42
```

To group multiple specs in a logically related group of specs, we use
`describe`. This creates a new spec which represents the named group.

```purescript
describe "MyModule" do
  it "..." do
    ...
  it "..." do
    ...
  it "..." do
    ...
```

Spec groups can be nested in multiple levels, creating a hierarchy of named
groups.

```purescript
describe "MyModule" $
  describe "SubModule" $
    describe "Database" do
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

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = launchAff_ $ run [consoleReporter] do
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
      pending "feature complete"
    describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports streaming reporters" $ pure unit
      it "supports async specs" do
        res <- delay (Milliseconds 100.0) $> "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.12.x compatible" $ pure unit
```

## Combining Specs

You can split specs into multiple files and combine them using regular monadic
bind, e.g. with `do` expressions.

```purescript
baseSpecs = do
  mathSpec
  stringsSpec
  arraySpec
  ...
```

This is often used to combine all specs into a single spec that can be passed
to the test runner, if not using [purescript-spec-discovery](https://github.com/purescript-spec/purescript-spec-discovery).

## Running A Subset of the Specs

Sometimes you do not wish to run all specs. It might be that you are working
on a certain feature, and only want to see the results for the relevant tests.
It can also be that some spec takes a lot of time, and you wish to exclude it
temporarily. By using `itOnly` instead of the regular `it`, the test runner
includes only that spec.

```purescript
describe "My API" do
  itOnly "does feature X" ... -- only this spec will run
  it "does things that takes a lot of time"
```

Similar to `itOnly`, `describeOnly` makes the runner include only that group.

```purescript
describe "Module" do
  describeOnly "Sub Module A" -- only this group will run
    it "does feature X" ...
  describe "Sub Module B"
    it "does feature Y" ...
```

## QuickCheck

You can use [QuickCheck](https://github.com/purescript/purescript-quickcheck)
together with the [purescript-spec-quickcheck](https://github.com/purescript-spec/purescript-spec-quickcheck)
adapter to get nice output formatting for QuickCheck tests.


## Parallel spec execution

You can use `parallel` to mark specs for parallel execution. This is useful
if you want to speed up your tests by not waiting for some async action
to resolve. so if you have:

```purescript
describe "delay" do
  it "proc 1" do
    delay $ Milliseconds 500.0
  it "proc 2" do
    delay $ Milliseconds 500.0
  it "proc 3" do
    delay $ Milliseconds 1000.0
```

It would take `2000 ms` to finish. But, by sticking in `parallel`, it would take `1000 ms`:

```diff
- describe "delay" do
+ describe "delay" $ parallel do
```

**NOTE** that if you are logging things to console, by using `parallel`
order of log messages will be mixed. For example if you had:

```purescript
describe "delay" do
  it "proc 1" do
    log $ "start 1"
    delay $ Milliseconds 500.0
    log $ "end 1"
  it "proc 2" do
    log $ "start 2"
    delay $ Milliseconds 500.0
    log $ "end 2"
  it "proc 3" do
    log $ "start 3"
    delay $ Milliseconds 1000.0
    log $ "end 3"
```

you would see messages in this order:

```
start 1
end 1
start 2
end 2
start 3
end 3
```

but if you have used `parallel` then messages will come in this order:

```
start 1
start 2
start 3
end 1
end 2
end 3
```

`purescript-spec` itself is not providing any specific solution for this
issue but you can take a look at [/test/Test/Spec/HoistSpec.purs](https://github.com/purescript-spec/purescript-spec/blob/master/test/Test/Spec/HoistSpec.purs) 
for some inspiration.