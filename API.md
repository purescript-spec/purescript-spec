# Module Documentation

## Module Control.Monad.Extras

#### `mapM_`

``` purescript
mapM_ :: forall m a b. (Monad m) => (a -> m b) -> [a] -> m Unit
```



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



## Module Test.Spec.Reporter

#### `debug`

``` purescript
debug :: forall v. v -> v
```


#### `showAssertionError`

``` purescript
showAssertionError :: Error -> String
```


#### `Entry`

``` purescript
data Entry
  = Describe [S.Name]
  | It S.Name S.Result
  | Pending S.Name
```


#### `eqEntry`

``` purescript
instance eqEntry :: Eq Entry
```


#### `showEntry`

``` purescript
instance showEntry :: Show Entry
```


#### `printEntry`

``` purescript
printEntry :: forall r. Entry -> Eff (trace :: Trace | r) Unit
```


#### `countDescribes`

``` purescript
countDescribes :: [Entry] -> Number
```


#### `collapse`

``` purescript
collapse :: S.Group -> [Entry]
```


#### `report`

``` purescript
report :: forall r. [S.Group] -> Eff (trace :: Trace | r) Unit
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
describe :: forall r. String -> Runner (trace :: Trace | r) Unit -> Runner (trace :: Trace | r) Unit
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


#### `collect`

``` purescript
collect :: forall r. Runner (trace :: Trace | r) Unit -> Eff (trace :: Trace | r) [Group]
```


#### `suite`

``` purescript
suite :: forall r. Runner (trace :: Trace | r) Unit -> Eff (trace :: Trace | r) Unit
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


#### `printPending`

``` purescript
printPending :: forall r. Number -> Eff (trace :: Trace | r) Unit
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


#### `showResult`

``` purescript
instance showResult :: Show Result
```


#### `eqResult`

``` purescript
instance eqResult :: Eq Result
```


#### `showGroup`

``` purescript
instance showGroup :: Show Group
```


#### `eqGroup`

``` purescript
instance eqGroup :: Eq Group
```




