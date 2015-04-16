SRC = $(shell find src -type f -name '*.purs')
LIB = $(shell find bower_components/purescript-*/src -type f -name '*.purs')

build:
	psc-make $(SRC) $(LIB)

docs:
	psc-docs $(SRC) > API.md
