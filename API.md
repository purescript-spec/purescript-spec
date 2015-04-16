# Module Documentation

## Module Test.Spec.Assertions

#### `equals`

``` purescript
equals :: forall r t. (Show t, Eq t) => t -> t -> Eff (err :: Exception | r) Unit
```



## Module Termino.Runner

#### `Runner`

``` purescript
type Runner r t = StateT [Group] (Eff r) t
```


#### `setAttr`

``` purescript
setAttr :: forall e. Number -> Eff (trace :: Trace | e) Unit
```


#### `reset`

``` purescript
reset :: forall e. Eff (trace :: Trace | e) Unit
```


#### `withAttrs`

``` purescript
withAttrs :: forall r. [Number] -> Eff (trace :: Trace | r) Unit -> Eff (trace :: Trace | r) Unit
```


#### `describe`

``` purescript
describe :: forall r. String -> Runner r Unit -> Runner r Unit
```


#### `run`

``` purescript
run :: forall r. String -> Eff (err :: Exception | r) Unit -> Runner r Group
```


#### `pending`

``` purescript
pending :: forall r. String -> Runner r Unit
```


#### `it`

``` purescript
it :: forall r. String -> Eff (err :: Exception | r) Unit -> Runner r Unit
```


#### `mapM_`

``` purescript
mapM_ :: forall m a b. (Monad m) => (a -> m b) -> [a] -> m Unit
```


#### `showAssertionError`

``` purescript
showAssertionError :: Error -> String
```


#### `printGroup`

``` purescript
printGroup :: forall r. Group -> Eff (trace :: Trace | r) Unit
```


#### `suite`

``` purescript
suite :: forall r. Runner (trace :: Trace | r) Unit -> Eff (trace :: Trace | r) Unit
```



## Module Test.Spec

#### `Name`

``` purescript
type Name = String
```


#### `Result`

``` purescript
data Result
  = Success 
  | Failure Error
```


#### `Group`

``` purescript
data Group
  = Describe Name [Group]
  | It Name Result
  | Pending Name
```




