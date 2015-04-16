TESTS = $(shell find tests -type f -name '*.purs')
SRC = $(shell find src -type f -name '*.purs')
LIB = $(shell find bower_components/purescript-*/src -type f -name '*.purs')

build:
	psc-make $(SRC) $(LIB)

docs:
	psc-docs $(SRC) > API.md

test:
	psc-make $(TESTS) $(SRC) $(LIB)
	@NODE_PATH=output node -e "require('Main').main();"

watch-test:
	nodemon --watch src --watch tests -e purs --exec make test
