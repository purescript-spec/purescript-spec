module Data.XML.PrettyPrintSpec where

import Test.Spec
import Test.Spec.Node
import Test.Spec.Assertions
import Test.Spec.Fixtures
import qualified Data.XML as XML
import qualified Data.XML.PrettyPrint as PP

prettyPrintSpec =
  describe "Data" $
    describe "XML" do
      describe "print"  do
        let doc = XML.Document "1.0" "UTF-8"
            doctype = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

        it "prints empty root tag" do
          let s = PP.print 2 $ doc $ XML.Element "test" [] []
          s `shouldEqual` (doctype ++ "<test></test>\n")

        it "prints root tag attrs" do
          let s = PP.print 2 $ doc $ XML.Element "test" [XML.Attr "foo" "bar"] []
          s `shouldEqual` (doctype ++ "<test foo=\"bar\"></test>\n")

        it "escapes attr value" do
          let s = PP.print 2 $ doc $ XML.Element "test" [XML.Attr "foo" "<xml/>"] []
          s `shouldEqual` (doctype ++ "<test foo=\"&lt;xml/&gt;\"></test>\n")

        it "prints nested tags" do
          let s = PP.print 2 $ doc $ XML.Element "test" [] [XML.Element "foo" [] []]
          s `shouldEqual` (doctype ++ "<test>\n  <foo></foo>\n</test>\n")

        it "prints text in tag" do
          let s = PP.print 2 $ doc $ XML.Element "test" [] [XML.Text "foo\nbar"]
          s `shouldEqual` (doctype ++ "<test>\n  foo\n  bar\n</test>\n")

        it "escapes text in tag" do
          let s = PP.print 2 $ doc $ XML.Element "test" [] [XML.Text "<xml-again />"]
          s `shouldEqual` (doctype ++ "<test>\n  &lt;xml-again /&gt;\n</test>\n")

