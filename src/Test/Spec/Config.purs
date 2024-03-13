module Test.Spec.Config
  ( Config
  , TreeFilter(..)
  , defaultConfig
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Test.Spec (SpecTree)

-- | Configuration options for the test runner.
-- |
-- | This type defines various settings that control the behavior of the test runner.
-- |
-- | Config has the following fields
-- | - `slow`: Threshold of time beyond which a test is considered "slow".
-- | - `timeout`: An optional timeout, applied to each individual test. When omitted, tests are allowed to run forever.
-- | - `exit`: When `true`, the runner will exit the Node process after running tests. If `false`, the runner will merely return test results.
-- | - `failFast`: When `true`, first failed test stops the whole run.
-- | - `filterTree`: The spec tree goes through this function before execution. Can be used to filter out test cases, rearrange, annotate, etc.
type Config =
  { slow :: Milliseconds
  , timeout :: Maybe Milliseconds
  , exit :: Boolean
  , failFast :: Boolean
  , filterTree :: TreeFilter
  }

newtype TreeFilter = TreeFilter (∀ g i. Array (SpecTree g i) -> Array (SpecTree g i))

defaultConfig :: Config
defaultConfig =
  { slow: Milliseconds 75.0
  , timeout: Just $ Milliseconds 2000.0
  , exit: true
  , failFast: false
  , filterTree: TreeFilter identity
  }
