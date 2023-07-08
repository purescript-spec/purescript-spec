module Test.Spec.Config
  ( Config
  , defaultConfig
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

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
  }

defaultConfig :: Config
defaultConfig =
  { slow: Milliseconds 75.0
  , timeout: Just $ Milliseconds 2000.0
  , exit: true
  , failFast: false
  }
