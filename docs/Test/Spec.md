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

##### Instances
``` purescript
instance showResult :: Show Result
instance eqResult :: Eq Result
```

#### `Group`

``` purescript
data Group
  = Describe Name (Array Group)
  | It Name Result
  | Pending Name
```

##### Instances
``` purescript
instance showGroup :: Show Group
instance eqGroup :: Eq Group
```

#### `Spec`

``` purescript
type Spec r t = StateT (Array Group) (Aff r) t
```

#### `describe`

``` purescript
describe :: forall r. String -> Spec r Unit -> Spec r Unit
```

#### `pending`

``` purescript
pending :: forall r. String -> Spec r Unit
```

#### `it`

``` purescript
it :: forall r. String -> Aff r Unit -> Spec r Unit
```

#### `collect`

``` purescript
collect :: forall r. Spec r Unit -> Aff r (Array Group)
```


