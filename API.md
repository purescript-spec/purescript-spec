# Module Documentation

## Module Control.Monad.Extras

#### `mapM_`

``` purescript
mapM_ :: forall m a b. (Monad m) => (a -> m b) -> [a] -> m Unit
```



## Module Data.XML.PrettyPrint

#### `Indent`

``` purescript
type Indent = Number
```


#### `CurrentIndent`

``` purescript
type CurrentIndent = Number
```


#### `PrinterState`

``` purescript
data PrinterState
  = PrinterState Indent CurrentIndent
```

The number of spaces in an indent and the current number of indents made.
`PrinterState 2 6` represents 12 spaces.

#### `Printer`

``` purescript
type Printer v = StateT PrinterState (Writer String) v
```


#### `indent`

``` purescript
indent :: Printer Unit
```


#### `dedent`

``` purescript
dedent :: Printer Unit
```


#### `indentSpaces`

``` purescript
indentSpaces :: Printer String
```


#### `appendLine`

``` purescript
appendLine :: String -> Printer Unit
```


#### `enclosed`

``` purescript
enclosed :: String -> String -> String -> String
```


#### `openTag`

``` purescript
openTag :: String -> [Attr] -> String
```


#### `closeTag`

``` purescript
closeTag :: String -> String
```


#### `escape`

``` purescript
escape :: String -> String
```


#### `printNode`

``` purescript
printNode :: Node -> Printer Unit
```


#### `showAttrs`

``` purescript
showAttrs :: [Attr] -> String
```


#### `printDocument`

``` purescript
printDocument :: Document -> Printer Unit
```


#### `print`

``` purescript
print :: Indent -> Document -> String
```



## Module Data.XML

#### `Version`

``` purescript
type Version = String
```

#### `Encoding`

``` purescript
type Encoding = String
```


#### `Document`

``` purescript
data Document
  = Document Version Encoding Node
```


#### `TagName`

``` purescript
type TagName = String
```


#### `Node`

``` purescript
data Node
  = Element TagName [Attr] [Node]
  | Text String
  | Comment String
```


#### `Attr`

``` purescript
data Attr
  = Attr String String
```



## Module Test.Spec.Assertions.String

#### `shouldContain`

``` purescript
shouldContain :: forall r. String -> String -> Aff r Unit
```



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



## Module Test.Spec.Errors

#### `errorMessage`

``` purescript
errorMessage :: Error -> String
```


#### `errorName`

``` purescript
errorName :: Error -> String
```


#### `errorStackTrace`

``` purescript
errorStackTrace :: Error -> String
```



## Module Test.Spec.Node

#### `Process`

``` purescript
data Process :: !
```


#### `runNode`

``` purescript
runNode :: forall e r. [[Group] -> Eff (trace :: Trace, process :: Process | e) Unit] -> Spec (trace :: Trace, process :: Process | e) Unit -> Eff (trace :: Trace, process :: Process | e) Unit
```



## Module Test.Spec.Reporter.Console

#### `consoleReporter`

``` purescript
consoleReporter :: forall e. Reporter (trace :: Trace | e)
```



## Module Test.Spec.Reporter.Xunit

#### `xunitReporter`

``` purescript
xunitReporter :: forall e. FilePath -> Reporter (err :: Exception, fs :: FS | e)
```

Outputs an XML file at the given path that can be consumed by Xunit
readers, e.g. the Jenkins plugin.


## Module Test.Spec.Reporter

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


#### `Reporter`

``` purescript
type Reporter e = [S.Group] -> Eff e Unit
```


#### `collapse`

``` purescript
collapse :: S.Group -> [Entry]
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


#### `successful`

``` purescript
successful :: [Group] -> Boolean
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


#### `Spec`

``` purescript
type Spec r t = StateT [Group] (Aff r) t
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
collect :: forall r. Spec r Unit -> Aff r [Group]
```




