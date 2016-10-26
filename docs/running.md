# Running

When you have a spec, you need a runner to actually run it and get the results.
PureScript Spec comes with a default runner, `run`, which takes an array of
*reporters* and a spec to run. What you get back is a test-running program of
type `Eff r ()`. The effect rows in `r` depend on what you do in your specs and
what reporters you are using. The program can be run using
[Pulp](https://github.com/bodil/pulp).

```bash
pulp test
```

If you're not using pulp, you can compile the test program using `psc`. The
following command compiles all PureScript modules in `test` and `src`.


```bash
psc -o output/tests 'test/**/*.purs' 'src/**/*.purs'
```

After that has finished, you can run the test program using NodeJS.

```
NODE_PATH=output/tests node -e "require('Test.Main').main();"
```

## Reporters

Reporters can be passed to the runner, e.g. `run [reporter1, ..., reporterN]
spec`. Currently there are these reporters available:

* [Test.Spec.Reporter.Console.consoleReporter](https://pursuit.purescript.org/packages/purescript-spec/0.10.0/docs/Test.Spec.Reporter.Console#v:consoleReporter)
* [purescript-spec-reporter-xunit](https://github.com/owickstrom/purescript-spec-reporter-xunit)

## Automatically Discovering Specs

If you are running your specs in an NodeJS environment, e.g. with `pulp test`,
you can automatically scan for spec modules using [purescript-spec-discovery](https://github.com/owickstrom/purescript-spec-discovery).
Then your `main` function can be as simple as:

```purescript
main = discover "My\\.Package\\..*Spec" >>= run [consoleReporter]
```

All modules matching the pattern, that has a `spec :: Spec r ()` definition
will be combined into a single large spec by `discover`.

## Browser Testing

You can run tests in a browser environment, instead of NodeJS, using `mocha`
or `karma`. For more information, see [purescript-spec-mocha](
https://github.com/owickstrom/purescript-spec-mocha).
