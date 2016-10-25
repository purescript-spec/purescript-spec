# purescript-spec [![Build Status](https://travis-ci.org/owickstrom/purescript-spec.svg?branch=master)](https://travis-ci.org/owickstrom/purescript-spec)

purescript-spec is a simple testing framework for Purescript. It is
inspired by [hspec](http://hspec.github.io/).

<img src="https://raw.githubusercontent.com/owickstrom/purescript-spec/master/example.png" width="400" />

## Usage

```bash
bower install purescript-spec
```

### Example

The purescript-spec DSL features a number functions that you can use to
write and organize specs. The specs shown in the image above looks like
this:

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

For more details on DSL functions, see the `Test.Spec` module
on [Pursuit](https://pursuit.purescript.org/packages/purescript-spec).

### Combining Specs

You can split test specs into multiple files and combine
them in `Test.Main` using a do expression.

```purescript
main = run [consoleReporter] do
  mathSpec
  stringsSpec
  arraySpec
  ...
```

### Reporters

Reporters can be passed to the runner, e.g. `run [reporter1, ..., reporterN]
spec`. Currently there are these reporters available:

* [`Test.Spec.Reporter.Console.consoleReporter`](docs/Test/Spec/Reporter/Console.md#consolereporter)
* [purescript-spec-reporter-xunit](https://github.com/owickstrom/purescript-spec-reporter-xunit)

### Async

Specs can be async as the test body type of `it` is
[forall e. Aff e Unit](https://github.com/slamdata/purescript-aff)

### Running Tests

When using `run` you can follow the conventions of
[pulp](https://github.com/bodil/pulp) and run your tests with `pulp test`.

If you're not using pulp you can compile using `psc` and run the compiled
Javascript with `node`.

```bash
# compile with psc (modify the command to include all your sources)
psc -o output/tests 'test/**/*.purs' 'src/**/*.purs' --ffi 'src/**/*.js'
# run using node
NODE_PATH=output/tests node -e "require('Test.Main').main();"
```

### Browser Testing

Compile and bundle your tests using `pulp browserify -I test --main Test.Main`
and run the bundled Javascript using `mocha` or `karma`. For more information,
see [purescript-spec-mocha](
https://github.com/owickstrom/purescript-spec-mocha).

## QuickCheck

You can use [QuickCheck](https://github.com/purescript/purescript-quickcheck)
together with the [`purescript-spec-quickcheck`](https://github.com/owickstrom/purescript-spec-quickcheck)
adapter to get nice output formatting for QuickCheck tests.

## API Documentation

See the [docs on Pursuit](https://pursuit.purescript.org/packages/purescript-spec).

## Build

This projects uses [pulp](https://github.com/bodil/pulp) for most of its
build. For stuff not supported by pulp we use Make.

```bash
# Make the library
pulp build
# Run tests
pulp test
# Generate docs
pulp docs
```

### Generate Example

Generating the `example.png` requires:

* make
* phantomjs
* aha
* imagemagick

```bash
make example.png
```

## CTags

This target generates a CTags file `tags`.

```bash
make ctags
```

## Changelog

* **0.10.0**
  * Defer test execution with Aff to have greater control. Test runners can
    either collect and run all tests, and then report their results, using
    the provided `run` and `Reporter` interface, or they can implement the
    test run in another way by `collect`ing a `Spec` and running the `Aff`s in
    some special way.
* **0.9.0**
  * Upgrade dependencies to 2.x
  * Require PureScript 0.10.x
* **0.8.0**
  * Compatibility with PureScript 0.9.1
* **0.7.5**
  * Bump dependency versions.
* **0.7.4**
  * Use `purescript-node-process` instead of custom PROCESS effect.
* **0.7.2**
  * Update purescript-strings for PS 0.7.4 compatibility.
  * Make all dependencies flexible.
  * Fix case statements to remove warnings.
* **0.7.1**
  * Workaround for escape sequence bug in `psc-bundle`, see:
    * https://github.com/owickstrom/purescript-spec/issues/12
    * https://github.com/purescript/purescript/issues/1265
* **0.7.0**
  * Rename `runNode` to `run` and place it in `Test.Spec.Runner`.
  * Support browser testing.
* **0.6.2**
  * Add more assertions.
* **0.6.1**
  * Fix bug in `shouldContain` assertion for strings.
* **0.6.0**
  * Adapt for PureScript 0.7 compatibility.
* **0.5.0**
  * Make reporters pluggable.
* **0.4.0**
  * Add async support in `it` using `Aff`.

## Contribute

If you have any issues or possible improvements please file them as
[GitHub Issues](https://github.com/owickstrom/purescript-spec/issues). Pull
requests are encouraged.

## License

[MIT License](LICENSE.md).
