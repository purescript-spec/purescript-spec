# purescript-spec [![Build Status](https://travis-ci.org/owickstrom/purescript-spec.svg?branch=master)](https://travis-ci.org/owickstrom/purescript-spec)

purescript-spec is a simple testing framework for Purescript using NodeJS. It's
inspired by [hspec](http://hspec.github.io/).

<img src="https://raw.githubusercontent.com/owickstrom/purescript-spec/master/example.png" width="400" />

## Usage

```bash
bower install purescript-spec
```

### Example

The specs shown in the image above:

```purescript
module Main where

import Prelude

import Control.Monad.Aff
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "What is it?" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
    describe "Features" do
      it "runs in NodeJS" $ return unit
      it "runs in the browser" $ return unit
      it "supports async specs" do
        res <- later' 100 $ return "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.7 compatible" $ return unit
```

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
* [purescript-spec-reporter-mocha](https://github.com/owickstrom/purescript-spec-reporter-mocha)

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
see [purescript-spec-reporter-mocha](
https://github.com/owickstrom/purescript-spec-reporter-mocha).

## QuickCheck

You can use [QuickCheck](https://github.com/purescript/purescript-quickcheck)
together with the [`purescript-spec-quickcheck`](https://github.com/owickstrom/purescript-spec-quickcheck)
adapter to get nice output formatting for QuickCheck tests.

## API

- [Test.Spec](docs/Test/Spec.md)
- [Test.Spec.Assertions](docs/Test/Spec/Assertions.md)
- [Test.Spec.Assertions.String](docs/Test/Spec/Assertions/String.md)
- [Test.Spec.Assertions.Aff](docs/Test/Spec/Assertions/Aff.md)
- [Test.Spec.Console](docs/Test/Spec/Console.md)
- [Test.Spec.Errors](docs/Test/Spec/Errors.md)
- [Test.Spec.Runner](docs/Test/Spec/Runner.md)
- [Test.Spec.Reporter](docs/Test/Spec/Reporter.md)
- [Test.Spec.Reporter.Console](docs/Test/Spec/Reporter/Console.md)
- [Test.Spec.Summary](docs/Test/Spec/Summary.md)

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
