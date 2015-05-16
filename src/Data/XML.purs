module Data.XML where

-- <?xml version="1.0" encoding="UTF-8"?>
-- <testsuite name="nosetests" tests="1" errors="1" failures="0" skip="0">
--   <testcase classname="path_to_test_suite.TestSomething"
--               name="test_it" time="0">
--     <error type="exceptions.TypeError" message="oops, wrong type">
--       Traceback (most recent call last):
--       ...
--       TypeError: oops, wrong type
--     </error>
--   </testcase>
-- </testsuite>

type Version = String
type Encoding = String
data Document = Document Version Encoding Node

type TagName = String
data Node = Element TagName [Attr] [Node]
          | Text String
          | Comment String

data Attr = Attr String String
