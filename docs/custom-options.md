---
title: Adding command-line options
parent: running
---

## What if I want to take my own command-line options?

You definitely can! The `runSpecAndExitProcess'` function takes a
`TestRunConfig` record, which you can extract from command-line options via the
`fromCommandLine'` function. `TestRunConfig` is an open record, so you can add
your own fields to it, and pass your own command-line parser to
`fromCommandLine'`. Then you can use the field for whatever purpose you need.

```haskell
import Options.Applicative as Opt
import Record (merge)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Config

type MyTestConfig =
  { onlyIntegrationTests :: Boolean
  | Config.TestRunConfigRow ()
  }

main :: Effect Unit
main = do
  config <- Config.fromCommandLine' defaultConfig $ Config.commandLineOptionParsers <> [integrationOnlyParser]

  let spec =
        | config.onlyIntegrationTests = integrationTests
        | otherwise = unitTests *> integrationTests

  runSpecAndExitProcess'
    { defaultConfig: config  -- Use this config to run the tests.
    , parseCLIOptions: false -- We already parsed them, no need to do it again.
    }
    [consoleReporter]
    spec

integrationOnlyParser :: Config.OptionParser MyTestConfig
integrationOnlyParser = ado
  x <- Opt.switch $ fold
    [ Opt.long "only-integration-tests"
    , Opt.help "run only integration tests"
    ]

  in \r -> r { onlyIntegrationTests = x }

defaultConfig :: MyTestConfig
defaultConfig = Config.defaultConfig `merge` { onlyIntegrationTests: false }
```
