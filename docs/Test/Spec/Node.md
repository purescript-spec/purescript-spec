## Module Test.Spec.Node

#### `Process`

``` purescript
data Process :: !
```

#### `runNode`

``` purescript
runNode :: forall e r. Array (Reporter (process :: Process, console :: CONSOLE | e)) -> Spec (process :: Process, console :: CONSOLE | e) Unit -> Eff (process :: Process, console :: CONSOLE | e) Unit
```


