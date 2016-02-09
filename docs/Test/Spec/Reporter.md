## Module Test.Spec.Reporter

#### `Entry`

``` purescript
data Entry
  = Describe (Array Name)
  | It Name Result
  | Pending Name
```

##### Instances
``` purescript
Eq Entry
Show Entry
```

#### `Reporter`

``` purescript
type Reporter e = Array Group -> Eff e Unit
```

#### `collapse`

``` purescript
collapse :: Group -> Array Entry
```


