OUTPUT = output

SRC = $(shell find src -type f -name '*.purs')
LIB = $(shell find bower_components/purescript-*/src -type f -name '*.purs')

TESTS = $(shell find tests -type f -name '*.purs')

EXAMPLES = $(shell find examples -type f -name '*.purs')
EXAMPLE_OUT=/tmp/purescript-spec-example-output.out
EXAMPLE_HTML=/tmp/purescript-spec-example-output.html

build: $(OUTPUT)
	psc-make -o output/lib $(SRC) $(LIB)

$(OUTPUT):
	mkdir -p $@

docs:
	psc-docs $(SRC) > API.md

build-examples: $(OUTPUT)
	psc-make -o $(OUTPUT)/examples $(EXAMPLES) $(SRC) $(LIB)

run-examples: build-examples
	@NODE_PATH=$(OUTPUT)/examples node -e "require('Main').main();"

example.png: build-examples
	@NODE_PATH=$(OUTPUT)/examples node -e "require('Main').main();" > $(EXAMPLE_OUT)
	aha -f $(EXAMPLE_OUT) > $(EXAMPLE_HTML)
	phantomjs tools/rasterize.js $(EXAMPLE_HTML) example.png 200 2
	convert example.png -trim example.png

watch-examples:
	nodemon --watch src --watch examples -e purs --exec make run-examples

run-tests:
	psc-make -o $(OUTPUT)/tests $(TESTS) $(SRC) $(LIB)
	@NODE_PATH=$(OUTPUT)/tests node -e "require('Main').main();"

watch-tests:
	nodemon --watch src --watch tests -e purs --exec make run-tests
