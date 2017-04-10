# PureScript Spec [![Build Status](https://travis-ci.org/owickstrom/purescript-spec.svg?branch=master)](https://travis-ci.org/owickstrom/purescript-spec)

PureScript Spec is a simple testing framework for Purescript, inspired by
[hspec](http://hspec.github.io/).

<img src="https://raw.githubusercontent.com/owickstrom/purescript-spec/master/example.png" width="400" />

## Documentation

* [PureScript Spec - The Guide](http://purescript-spec.wickstrom.tech) explains
in detail how to use purescript-spec. **You should probably start here.**
* [purescript-spec on Pursuit](https://pursuit.purescript.org/packages/purescript-spec)
  features version information and API documentation.
* [BUILD.md](BUILD.md) lists build instructions for this project. Most
  likely not interesting unless your hacking on PureScript Spec itself.

## Changelog

* **0.13.0**
  * Upgrade to PureScript 0.11.x, which is non-backwards compatible to 0.10.x.
* **0.12.4**
  * Upgrade to purescript-pipes 2.1.0
* **0.12.3**
  * Export and document `run'` function (for configuration), fixes #38
* **0.12.2**
  * Reexport common reporters in `Test.Spec.Reporter` module, fixes #37
* **0.12.1**
  * Move array of results to runner event, fixes #36
* **0.12.0**
  * Again support multiple reporters, fixes #33
* **0.11.0**
  * Publish *The Guide* on GitHub pages
  * Collapse result entries in Maps to deduplicate describes, fixes #9
  * Reimplement Node reporters and runner using purescript-pipes. An upgrade
    might require a change in your `main` type signature. The type
    `Test.Spec.Runner.RunnerEffects` makes it more convenient to specify all
    effect rows:
    * When using regular specs, use `main :: Eff (RunnerEffects ()) Unit`
    * When using purescript-spec-discovery, use `main :: Eff (RunnerEffects (fs :: FS)) Unit`
* **0.10.0**
  * Defer test execution with Aff to have greater control. Test runners can
    either collect and run all tests, and then report their results, using
    the provided `run` and `Reporter` interface, or they can implement the
    test run in another way by `collect`ing a `Spec` and running the `Aff`s in
    some special way.
* **0.9.0**
  * Upgrade dependencies to 2.x
  * Require PureScript 0.10.x
* **0.8.0**
  * Compatibility with PureScript 0.9.1
* **0.7.5**
  * Bump dependency versions.
* **0.7.4**
  * Use `purescript-node-process` instead of custom PROCESS effect.
* **0.7.2**
  * Update purescript-strings for PS 0.7.4 compatibility.
  * Make all dependencies flexible.
  * Fix case statements to remove warnings.
* **0.7.1**
  * Workaround for escape sequence bug in `psc-bundle`, see:
    * https://github.com/owickstrom/purescript-spec/issues/12
    * https://github.com/purescript/purescript/issues/1265
* **0.7.0**
  * Rename `runNode` to `run` and place it in `Test.Spec.Runner`.
  * Support browser testing.
* **0.6.2**
  * Add more assertions.
* **0.6.1**
  * Fix bug in `shouldContain` assertion for strings.
* **0.6.0**
  * Adapt for PureScript 0.7 compatibility.
* **0.5.0**
  * Make reporters pluggable.
* **0.4.0**
  * Add async support in `it` using `Aff`.

## Contribute

If you have any issues or possible improvements please file them as
[GitHub Issues](https://github.com/owickstrom/purescript-spec/issues). Pull
requests are encouraged.

## License

[MIT License](LICENSE.md).

## Status

[![](https://codescene.io/projects/124/status.svg) Get more details at **codescene.io**.](https://codescene.io/projects/124/jobs/latest-successful/results)
