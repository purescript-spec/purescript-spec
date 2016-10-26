# Writing Specs

*TODO!*

## Asynchronous Specs

Specs can be async as the test body type of `it` is
[forall e. Aff e Unit](https://github.com/slamdata/purescript-aff)


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
