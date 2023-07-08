# Integration tests

This directory contains fixtures/assets for integration tests, in which we run
`spago test` on an example program and check that its output is as expected.

`env-template` contains a skeleton of a PureScript project that will be used as
context for executing examples.

`cases` contains multiple subdirectories, each representing a test case. Every
test case consists of a `Main.purs` file (containing the test program to be run
via `spago test`) and an `output.txt` file (containing the program's expected
output).
