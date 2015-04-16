# Module Documentation

## Module Test.Spec.Assertions

#### `shouldEqual`

``` purescript
shouldEqual :: forall r t. (Show t, Eq t) => t -> t -> Eff (err :: Exception | r) Unit
```


#### `shouldNotEqual`

``` purescript
shouldNotEqual :: forall r t. (Show t, Eq t) => t -> t -> Eff (err :: Exception | r) Unit
```



## Module Test.Spec.Console

#### `write`

``` purescript
write :: forall e. String -> Eff (trace :: Trace | e) Unit
```


#### `writeln`

``` purescript
writeln :: forall e. String -> Eff (trace :: Trace | e) Unit
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



## Module Test.Spec.Runner

#### `Process`

``` purescript
data Process :: !
```


#### `exit`

``` purescript
exit :: forall eff. Number -> Eff (process :: Process | eff) Unit
```


#### `Runner`

``` purescript
type Runner r t = StateT [Group] (Eff r) t
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
suite :: forall r. Runner (process :: Process, trace :: Trace | r) Unit -> Eff (process :: Process, trace :: Trace | r) Unit
```



## Module Test.Spec.Summary

#### `Summary`

``` purescript
data Summary
  = Count Number Number Number
```


#### `semigroupCount`

``` purescript
instance semigroupCount :: Semigroup Summary
```


#### `summarize`

``` purescript
summarize :: [Group] -> Summary
```


#### `pluralize`

``` purescript
pluralize :: String -> Number -> String
```


#### `printPassedFailed`

``` purescript
printPassedFailed :: forall r. Number -> Number -> Eff (trace :: Trace | r) Unit
```


#### `printSkipped`

``` purescript
printSkipped :: forall r. Number -> Eff (trace :: Trace | r) Unit
```


#### `printSummary'`

``` purescript
printSummary' :: forall r. Summary -> Eff (trace :: Trace | r) Unit
```


#### `printSummary`

``` purescript
printSummary :: forall r. [Group] -> Eff (trace :: Trace | r) Unit
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




