---
title: Running
nav_order: 2
---

1. TOC
{: toc }

## Running under Node

If your backend is JavaScript (which is the default), you can run your specs
with the Node-based runner `runSpecAndExitProcess` from the
[`spec-node`](https://pursuit.purescript.org/packages/purescript-spec-node)
package. It takes an array of *reporters* and a spec to run. What you get back
is a test-running program of type `Effect Unit`, which you can just make your
`main` function in your `Test.Main` module:

```haskell
module Test.Main where

import Test.Spec (Spec)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter (consoleReporter)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] spec

spec :: Spec Unit
spec = ...
```

**NOTE:** The "exit process" part is necessary, because a test program is
supposed to return an exit code to the OS, which in Node has to be done by
calling `process.exit()`.

After that you can run the test suite with Spago:

```bash
spago test
```

The test runner saves the last run results in a local file named `.spec-results`
(recommended to include in your `.gitignore`) and can take command-line options
that make authoring and debugging more convenient.

* `--example TEXT` or `-e TEXT` - run only tests whose names include the given
  text.
* `--example-matches REGEX` or `-E REGEX` - run only tests whose names match the
  given regex.
* `--fail-fast` - stop the run after first failure.
* `--only-failures` - run only tests that failed on previous run.
* `--next-failure` or `-n` - run only failed tests and stop on first failure.
  Equivalent to --fail-fast --only-failures
* `--timeout SECONDS` - timeout for each individual test case, in seconds.
* `--no-timeout` - each individual test case is allowed to run for as long as it
  wants.
* You can add your own options. See [adding custom options]({% link custom-options.md %}).

For example:

```bash
# Run only tests that mention loader in their first name
spago test -- --example "loader"

# Set timeout of 20 seconds and stop after first failure
spago test -- --fail-fast --timeout 20
```

**NOTE:** A test program using `runSpecAndExitProcess` cannot be browserified
and run in the browser, it requires Node. To run your tests in a browser, see
[Browser Testing](#browser-testing) below.

## Reporters

Reporters can be passed to the runner, e.g. `runSpec [reporter1, ..., reporterN]
spec`. Currently there are these reporters available:

* `consoleReporter` in `Test.Spec.Reporter.Console`
* `dotReporter` in `Test.Spec.Reporter.Dot`
* `specReporter` in `Test.Spec.Reporter.Spec`
* `tapReporter` in `Test.Spec.Reporter.Tap`
* [purescript-spec-reporter-xunit](https://github.com/purescript-spec/purescript-spec-reporter-xunit)

## Automatically Discovering Specs

If you are running your specs in an Node environment, e.g. with `spago test`,
you can automatically scan for spec modules using [purescript-spec-discovery](https://github.com/purescript-spec/purescript-spec-discovery).
Then your `main` function can be as simple as:

```haskell
main = discover "My\\.Package\\..*Spec" >>= runSpecAndExitProcess [consoleReporter]
```

All modules matching the pattern, that has a `spec :: Spec r ()` definition
will be combined into a single large spec by `discover`.

## Running in a browser

You can run tests in a browser environment, instead of Node, using `mocha`
or `karma`. For more information, see [purescript-spec-mocha](
https://github.com/purescript-spec/purescript-spec-mocha).
