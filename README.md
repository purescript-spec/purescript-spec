# purescript-spec

<img src="https://raw.githubusercontent.com/owickstrom/purescript-spec/master/example.png" width="400" />


## Usage

```bash
bower install purescript-spec
```

Then in a `Main.purs` file...

```purescript
module Main where

import Test.Spec
import Test.Spec.Runner
import Test.Spec.Assertions

main = suite do
  describe "Math" do
    describe "Addition" do
        it "does addition" do
          (1 + 1) `shouldEqual` 2
        it "fails as well" do
          (1 + 1) `shouldEqual` 3
    describe "Multiplication" do
      pending "will do multiplication in the future"
```

## API

See [API](API.md).

## Build

```bash
# Make the library
make
# Run tests
make run-tests
# Generate docs
make docs
```

### Generate Example

Generating the `example.png` requires:

* phantomjs
* aha
* imagemagick

```
make example.png
```

## License

[MIT License](LICENSE.md).
