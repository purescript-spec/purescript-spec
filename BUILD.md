## Build

This projects uses [pulp](https://github.com/bodil/pulp) for most of its
build. For stuff not supported by pulp we use Make and Pandoc.

```bash
# Make the library
pulp build
# Run tests
pulp test
# Generate docs, requires Pandoc
pulp docs && make docs
```

### Generate Example

Generating the `example.png` requires:

* make
* phantomjs
* aha
* imagemagick

```bash
make example.png
```

## CTags

This target generates a CTags file `tags`.

```bash
make ctags
```

