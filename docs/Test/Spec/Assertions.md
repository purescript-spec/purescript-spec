## Module Test.Spec.Assertions

#### `fail`

``` purescript
fail :: forall r. String -> Aff r Unit
```

#### `shouldEqual`

``` purescript
shouldEqual :: forall r t. (Show t, Eq t) => t -> t -> Aff r Unit
```

#### `shouldNotEqual`

``` purescript
shouldNotEqual :: forall r t. (Show t, Eq t) => t -> t -> Aff r Unit
```

#### `shouldContain`

``` purescript
shouldContain :: forall r f a. (Show a, Eq a, Show (f a), Foldable f) => f a -> a -> Aff r Unit
```


