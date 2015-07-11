OUTPUT = output

SRC = $(shell find src -type f -name '*.purs')
LIB = $(shell find bower_components/purescript-*/src -type f -name '*.purs')

TESTS = $(shell find test -type f -name '*.purs')

NODEMON=node_modules/.bin/nodemon

EXAMPLES = $(shell find examples -type f -name '*.purs')
EXAMPLE_OUT=/tmp/purescript-spec-example-output.out
EXAMPLE_CSS=/tmp/purescript-spec-example.css
EXAMPLE_HTML=/tmp/purescript-spec-example-output.html

build: $(OUTPUT)
	psc-make -o output/lib $(SRC) $(LIB)

$(OUTPUT):
	mkdir -p $@

docs:
	psc-docs $(SRC) > API.md

ctags:
	psc-docs --format ctags $(SRC) $(LIB) > tags

build-examples: $(OUTPUT)
	psc-make -o $(OUTPUT)/examples $(EXAMPLES) $(SRC) $(LIB)

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
	psc-make -o $(OUTPUT)/test $(TESTS) $(SRC) $(LIB)

run-tests: build-tests
	@NODE_PATH=$(OUTPUT)/test node -e "require('Main').main();"

watch-tests:
	$(NODEMON) -V --watch src --watch test -e purs --exec make run-tests
