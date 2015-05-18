module Test.Spec.Reporter.Xunit (
  xunitReporter
  )
  where

import Data.Maybe
import Data.Array (map)
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad
import qualified Test.Spec as S
import Test.Spec.Errors
import Test.Spec.Console
import Test.Spec.Summary
import Test.Spec.Reporter
import Node.Encoding
import Node.Path
import Node.FS (FS(..))
import Node.FS.Sync (writeTextFile, exists, unlink)
import Data.XML hiding (Encoding())
import Data.XML.PrettyPrint

encodeResult :: S.Result -> [Node]
encodeResult S.Success = []
encodeResult (S.Failure err) =
  [Element
    "error"
    [Attr "type" (errorName err), Attr "message" (errorMessage err)]
    [Text (errorStackTrace err)]]

encodeGroup :: S.Group -> Node
encodeGroup (S.Describe name groups) =
  Element "testsuite" [Attr "name" name] $ map encodeGroup groups
encodeGroup (S.It name result) =
  Element "testcase" [Attr "name" name] (encodeResult result)

encodeSuite :: [S.Group] -> Document
encodeSuite groups = Document "1.0" "UTF-8" $ Element "testsuite" [] $ map encodeGroup groups

removeIfExists :: forall e. FilePath -> Eff (fs :: FS, err :: Exception | e) Unit
removeIfExists path = do
  e <- exists path
  when e $ unlink path

-- | Outputs an XML file at the given path that can be consumed by Xunit
-- | readers, e.g. the Jenkins plugin.
xunitReporter :: forall e. FilePath -> Reporter (fs :: FS, err :: Exception | e)
xunitReporter path groups = do
  let xml = encodeSuite groups
      s = print 2 xml
  removeIfExists path
  writeTextFile UTF8 path s

