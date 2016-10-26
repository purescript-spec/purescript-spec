# Running

When using `run` you can follow the conventions of
[pulp](https://github.com/bodil/pulp) and run your tests with `pulp test`.

If you're not using pulp you can compile using `psc` and run the compiled
Javascript with `node`.

```bash
# compile with psc (modify the command to include all your sources)
psc -o output/tests 'test/**/*.purs' 'src/**/*.purs' --ffi 'src/**/*.js'
# run using node
NODE_PATH=output/tests node -e "require('Test.Main').main();"
```

## Reporters

Reporters can be passed to the runner, e.g. `run [reporter1, ..., reporterN]
spec`. Currently there are these reporters available:

* `Test.Spec.Reporter.Console.consoleReporter`
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

You can run tests in a browser environment, instead of NodeJS. Compile and
bundle your tests using `pulp browserify -I test --main Test.Main` and run the
bundled Javascript using `mocha` or `karma`. For more information, see
[purescript-spec-mocha]( https://github.com/owickstrom/purescript-spec-mocha).
