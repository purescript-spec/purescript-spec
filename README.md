# purescript-spec

## Usage

```bash
bower install purescript-spec
```

Then in a `Main.purs` in your tests directory...

```purescript
main = suite $
  describe "Core" do
    it "applies Text" do
      (runApply (Text "foo") (Buffer [] [])) `equals` (Buffer [Text "foo"] [])

    it "applies Backspace on Text" do
      (runApply (Escape Backspace) (Buffer [Text "foo"] [Text "bar"])) `equals` (Buffer [Text "fo"] [Text "bar"])

    it "applies Backspace on empty Text" do
      (runApply (Escape Backspace) (Buffer [Text ""] [Text "bar"])) `equals` (Buffer [] [Text "bar"])

    it "applies CarriageReturn on Text" do
      (runApply (Escape CarriageReturn) (Buffer [Text "foo"] [Text "bar"])) `equals` (Buffer [] [Text "foo", Text "bar"])

    it "applies CarriageReturn on multiple lines" do
      (runApply (Escape CarriageReturn) (Buffer [Text "foo"] [Text "bar"])) `equals` (Buffer [] [Text "foo", Text "bar"])
```
