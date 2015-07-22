## Module Test.Spec.Summary

#### `Summary`

``` purescript
data Summary
  = Count Int Int Int
```

##### Instances
``` purescript
instance semigroupCount :: Semigroup Summary
instance monoidCount :: Monoid Summary
```

#### `summarize`

``` purescript
summarize :: Array Group -> Summary
```

#### `successful`

``` purescript
successful :: Array Group -> Boolean
```


