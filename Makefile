EXAMPLES = $(shell find examples -type f -name '*.purs')
TESTS = $(shell find tests -type f -name '*.purs')
SRC = $(shell find src -type f -name '*.purs')
LIB = $(shell find bower_components/purescript-*/src -type f -name '*.purs')

build:
	psc-make $(SRC) $(LIB)

docs:
	psc-docs $(SRC) > API.md

run-examples:
	psc-make $(EXAMPLES) $(SRC) $(LIB)
	@NODE_PATH=output node -e "require('Main').main();"

watch-examples:
	nodemon --watch src --watch examples -e purs --exec make run-examples

run-tests:
	psc-make $(TESTS) $(SRC) $(LIB)
	@NODE_PATH=output node -e "require('Main').main();"

watch-tests:
	nodemon --watch src --watch tests -e purs --exec make run-tests
