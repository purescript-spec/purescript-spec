OUTPUT = output

SRC = 'src/**/*.purs'
LIB = 'bower_components/purescript-*/src/**/*.purs'
SRC_FFI = 'src/**/*.js'
LIB_FFI = 'bower_components/purescript-*/src/**/*.js'
TESTS = 'test/**/*.purs'
EXAMPLES = 'examples/Main.purs'

DEFAULT_PSC_ARGS = $(SRC) $(LIB) --ffi $(SRC_FFI) --ffi $(LIB_FFI)

NODEMON=node_modules/.bin/nodemon

EXAMPLE_OUT=/tmp/purescript-spec-example-output.out
EXAMPLE_CSS=/tmp/purescript-spec-example.css
EXAMPLE_HTML=/tmp/purescript-spec-example-output.html

build: $(OUTPUT)
	psc $(DEFAULT_PSC_ARGS) -o $(OUTPUT)/lib

$(OUTPUT):
	mkdir -p $@

docs:
	psc-docs $(SRC) > API.md

ctags:
	psc-docs --format ctags $(SRC) $(LIB) > tags

build-examples: $(OUTPUT)
	psc $(EXAMPLES) $(DEFAULT_PSC_ARGS) -o $(OUTPUT)/examples

run-examples: build-examples
	@NODE_PATH=$(OUTPUT)/examples node -e "require('Main').main();"

$(EXAMPLE_CSS): tools/styles.css
	cp tools/styles.css $(EXAMPLE_CSS)

example.png: build-examples $(EXAMPLE_CSS)
	@NODE_PATH=$(OUTPUT)/examples node -e "require('Main').main();" > $(EXAMPLE_OUT)
	aha -s -f $(EXAMPLE_OUT) | awk '/head/{print "<link rel=\"stylesheet\" href=\"$(EXAMPLE_CSS)\" \>"}1' > $(EXAMPLE_HTML)
	phantomjs tools/rasterize.js $(EXAMPLE_HTML) example.png 200 2
	convert example.png -trim example.png

$(NODEMON):
	npm install nodemon@1.2

watch-examples:
	$(NODEMON) --watch src --watch examples -e purs --exec make run-examples

build-tests: $(OUTPUT)
	psc $(TESTS) $(DEFAULT_PSC_ARGS) -o $(OUTPUT)/test

run-tests: build-tests
	@NODE_PATH=$(OUTPUT)/test node -e "require('Test.Main').main();"

watch-tests:
	$(NODEMON) -V --watch src --watch test -e purs --exec make run-tests
