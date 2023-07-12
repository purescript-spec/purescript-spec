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

type Config =
  { slow :: Milliseconds
  -- ^ Threshold of time beyond which a test is considered "slow".

  , timeout :: Maybe Milliseconds
  -- ^ An optional timeout, applied to each individual test. When omitted, tests
  -- are allowed to run forever.

  , exit :: Boolean
  -- ^ When `true`, the runner will exit the Node process after running tests.
  -- If `false`, the runner will merely return test results.

  , failFast :: Boolean
  -- ^ When `true`, first failed test stops the whole run.

  , filterTree :: TreeFilter
  -- ^ The spec tree goes through this function before execution. Can be used to
  -- filter out test cases, rearrange, annotate, etc.
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
