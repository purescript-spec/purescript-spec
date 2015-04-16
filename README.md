# purescript-spec

## Usage

```bash
bower install purescript-spec
```

Then in a `Main.purs` in your tests directory...

```purescript
main = suite $
  describe "Math" do
    it "works" do
      (1 + 1) `equals` 2
    it "does not work" do
      (1 + 1) `equals` 2
    pending "this is not used yet"
```

## API

See [API](API.md).
