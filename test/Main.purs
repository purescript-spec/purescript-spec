module Test.Main where

import Prelude

import Data.Array (fold)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Options.Applicative as Opt
import Record (merge)
import Test.Integration (integrationSpecs)
import Test.Spec.AssertionSpec (assertionSpec)
import Test.Spec.HoistSpec (hoistSpecSpec)
import Test.Spec.HookSpec (hookSpec)
import Test.Spec.ParallelSpec (parallelSpec)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Reporter.TeamCitySpec (teamcitySpec)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Config
import Test.Spec.RunnerSpec (runnerSpec)

main :: Effect Unit
main = launchAff_ do
  config <- liftEffect $
    Config.fromCommandLine' defaultConfig (Config.commandLineOptionParsers <> [debug])
    <#> _ { timeout = Just $ Milliseconds 30000.0 }
  integration <-
    integrationSpecs { debug: config.debug }
  liftEffect $
    runSpecAndExitProcess'
      { defaultConfig: config
      , parseCLIOptions: false
      }
      [specReporter] $
      pureSpecs *> integration
  where
    pureSpecs = do
      runnerSpec
      assertionSpec
      hookSpec
      hoistSpecSpec
      parallelSpec
      teamcitySpec

type Config = Config.TestRunConfig' (debug :: Boolean)

defaultConfig :: Config
defaultConfig = Config.defaultConfig `merge` { debug: false }

debug :: Config.OptionParser Config
debug = ado
  d <- Opt.switch $ fold
    [ Opt.long "debug"
    , Opt.help "Do not destroy temporary directory used for integration tests."
    ]

  in _ { debug = d }
