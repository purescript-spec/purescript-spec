## Module Test.Spec.Console

#### `setAttr`

``` purescript
setAttr :: forall e. Int -> Eff (console :: CONSOLE | e) Unit
```

#### `reset`

``` purescript
reset :: forall e. Eff (console :: CONSOLE | e) Unit
```

#### `withAttrs`

``` purescript
withAttrs :: forall r. Array Int -> Eff (console :: CONSOLE | r) Unit -> Eff (console :: CONSOLE | r) Unit
```


