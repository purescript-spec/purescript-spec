# purescript-spec

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

import Control.Monad.Aff
import Test.Spec (describe, pending, it)
import Test.Spec.Node
import Test.Spec.Assertions
import Test.Spec.Reporter.Console
import Test.QuickCheck

main = runNode [consoleReporter] do
  describe "purescript-spec" do
    describe "What is it?" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
    describe "Features" do
      it "run specs in NodeJS" $ return unit
      it "supports async specs" do
        res <- later' 100 $ return "Alligator"
        res `shouldEqual` "Alligator"
    describe "TODO" do
      pending "browser support!"
```

### Embedding Specs

In the example `additionSpec` is embedded into the `Math` specification. This
is useful if you want to split specifications into multiple files and combine
them in `Main`.

```purescript
main = runNode [consoleReporter] do
  mathSpec
  stringsSpec
  arraySpec
  ...
```

### Reporters

Reporters can be passed to the runner, e.g. `runNode [reporter1, ..., reporterN]
spec`. Currently there are these reporters available:

* [`Test.Spec.Reporter.Console.consoleReporter`](docs/Test/Spec/Reporter/Console.md#consolereporter)
* [purescript-spec-reporter-xunit](https://github.com/owickstrom/purescript-spec-reporter-xunit)

### Async

Specs can be async as the test body type of `it` is
[forall e. Aff e Unit](https://github.com/slamdata/purescript-aff)

### Running Tests

Run the test suite using `psc-make` and NodeJS. Note that `$TESTS`, `$SRC`
and `$LIB` contain all the Purescript source paths needed.

```bash
psc-make -o output/tests $TESTS $SRC $LIB
NODE_PATH=output/tests node -e "require('Main').main();"
```

## QuickCheck

You can use [QuickCheck](https://github.com/purescript/purescript-quickcheck)
together with the [`purescript-spec-quickcheck`](https://github.com/owickstrom/purescript-spec-quickcheck)
adapter to get nice output formatting for QuickCheck tests.

## API

- [Test.Spec](docs/Test/Spec.md)
- [Test.Spec.Assertions](docs/Test/Spec/Assertions.md)
- [Test.Spec.Assertions.String](docs/Test/Spec/Assertions/String.md)
- [Test.Spec.Console](docs/Test/Spec/Console.md)
- [Test.Spec.Errors](docs/Test/Spec/Errors.md)
- [Test.Spec.Node](docs/Test/Spec/Node.md)
- [Test.Spec.Reporter](docs/Test/Spec/Reporter.md)
- [Test.Spec.Reporter.Console](docs/Test/Spec/Reporter/Console.md)
- [Test.Spec.Summary](docs/Test/Spec/Summary.md)

## Build

```bash
# Make the library
make
# Run tests
make run-tests
# Generate docs
make docs
```

Alternatively, using [pulp](https://github.com/bodil/pulp):

```bash
# Build the library
pulp build
# Run tests
pulp test
# Generate docs
pulp docs
```

### Generate Example

Generating the `example.png` requires:

* phantomjs
* aha
* imagemagick

```
make example.png
```

## CTags

```bash
make ctags
```

## Changelog

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
