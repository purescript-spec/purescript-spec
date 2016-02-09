## Module Test.Spec.Runner

#### `Process`

``` purescript
data Process :: !
```

#### `run`

``` purescript
run :: forall e. Array (Reporter (process :: Process, console :: CONSOLE | e)) -> Spec (process :: Process, console :: CONSOLE | e) Unit -> Eff (process :: Process, console :: CONSOLE | e) Unit
```


