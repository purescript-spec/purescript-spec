# Running

When you have a spec, you need a runner to actually run it and get the results.
PureScript Spec comes with a NodeJS runner, `runSpec`, which takes an array of
*reporters* and a spec to run. What you get back is a test-running program of
type `Aff Unit`. The program can be run using
[Pulp](https://github.com/bodil/pulp).

```bash
pulp test
```

If you're not using pulp, you can compile the test program using `psc`. The
following command compiles all PureScript modules in `test` and `src`.


```bash
psc -o output 'test/**/*.purs' 'src/**/*.purs'
```

After that has finished, you can run the test program using NodeJS.

```
NODE_PATH=output node -e "require('Test.Main').main();"
```

**NOTE:** A test program using `Test.Spec.Runner.runSpec` cannot be browserified
and run in the browser, it requires NodeJS. To run your tests in a browser,
see [Browser Testing](#browser-testing) below.

## Reporters

Reporters can be passed to the runner, e.g. `runSpec [reporter1, ..., reporterN]
spec`. Currently there are these reporters available:

* `consoleReporter` in `Test.Spec.Reporter.Console`
* `dotReporter` in `Test.Spec.Reporter.Dot`
* `specReporter` in `Test.Spec.Reporter.Spec`
* `tapReporter` in `Test.Spec.Reporter.Tap`
* [purescript-spec-reporter-xunit](https://github.com/owickstrom/purescript-spec-reporter-xunit)

## Passing Runner Configuration

In addition to the regular `runSpec` function, there is also `runSpecT`, which also
takes `Config` record. also instead of `Spec Unit` it takes `SpecT Aff Unit m Unit`
and returns `m (Aff (Array (Tree Void Result)))`. if we specialize the `m` to `Identity`
then code will look like this:

```purescript
main = launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] mySpec
  where
    testConfig = { slow: 5000, timeout: Just 10000, exit: false }
```

## Automatically Discovering Specs

If you are running your specs in an NodeJS environment, e.g. with `pulp test`,
you can automatically scan for spec modules using [purescript-spec-discovery](https://github.com/owickstrom/purescript-spec-discovery).
Then your `main` function can be as simple as:

```purescript
main = discover "My\\.Package\\..*Spec" >>= runSpec [consoleReporter] >>> launchAff_
```

All modules matching the pattern, that has a `spec :: Spec r ()` definition
will be combined into a single large spec by `discover`.

## Browser Testing

You can run tests in a browser environment, instead of NodeJS, using `mocha`
or `karma`. For more information, see [purescript-spec-mocha](
https://github.com/owickstrom/purescript-spec-mocha).
