OUTPUT = output

SRC = 'src/**/*.purs'
LIB = 'bower_components/purescript-*/src/**/*.purs'
SRC_FFI = 'src/**/*.js'
LIB_FFI = 'bower_components/purescript-*/src/**/*.js'
example = 'example/Main.purs'

EXAMPLE_OUT=/tmp/purescript-spec-example-output.out
EXAMPLE_CSS=/tmp/purescript-spec-example.css
EXAMPLE_HTML=/tmp/purescript-spec-example-output.html

$(OUTPUT):
	mkdir -p $@

ctags:
	psc-docs \
		--format ctags \
		$(SRC) \
		$(LIB) \
		> tags

build-example: $(OUTPUT)
	psc \
		$(example) \
		$(SRC) \
		$(LIB) \
		--ffi $(SRC_FFI) \
		--ffi $(LIB_FFI) \
		-o $(OUTPUT)/example

run-example: build-example
	@NODE_PATH=$(OUTPUT)/example node -e "require('Main').main();"

$(EXAMPLE_CSS): tools/styles.css
	cp tools/styles.css $(EXAMPLE_CSS)

example.png: build-example $(EXAMPLE_CSS)
	@NODE_PATH=$(OUTPUT)/example node -e "require('Main').main();" > $(EXAMPLE_OUT)
	aha -s -f $(EXAMPLE_OUT) | awk '/head/{print "<link rel=\"stylesheet\" href=\"$(EXAMPLE_CSS)\" \>"}1' > $(EXAMPLE_HTML)
	phantomjs tools/rasterize.js $(EXAMPLE_HTML) example.png 200 2
	convert example.png -trim example.png
