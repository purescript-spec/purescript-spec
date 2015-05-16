module Test.Spec.Reporter.XunitSpec where

import Data.Either
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Trans
import Control.Monad.Error.Class
import Node.Encoding
import Node.Path
import Node.FS (FS(..))
import Node.FS.Sync (readTextFile, unlink)
import Test.Spec
import Test.Spec.Node
import Test.Spec.Assertions hiding (shouldContain)
import Test.Spec.Assertions.String
import Test.Spec.Fixtures
import Test.Spec.Reporter.Xunit

successResult = [Describe "a" [It "works" Success]]
failureResult = [Describe "a" [It "fails" $ Failure (error "OMG")]]

-- This is only needed for type inference.
catchFs :: forall e v. Eff (fs :: FS | e) v -> Aff (fs :: FS | e) v
catchFs e = liftEff $ e

xunitSpec = do
  describe "Test" $
    describe "Spec" $
      describe "Reporter" $
        describe "Xunit" do
          let path = "output/test.tmp.xml"
              doctype = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          it "reports success" do
            catchFs $ xunitReporter path successResult
            contents <- catchFs $ readTextFile UTF8 path
            contents`shouldEqual` (doctype ++ "<testsuite>\n  <testsuite name=\"a\">\n    <testcase name=\"works\"></testcase>\n  </testsuite>\n</testsuite>\n")
          it "reports failure" do
            catchFs $ xunitReporter path failureResult
            contents <- catchFs $ readTextFile UTF8 path
            contents `shouldContain` "<testcase name=\"fails\">"
            contents `shouldContain` "Error"

