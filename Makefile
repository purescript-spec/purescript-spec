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
	pulp build -I example --to $(OUTPUT)/example.js

run-example: build-example
	@NODE_PATH=$(OUTPUT) node -e "require('Main').main();"

$(EXAMPLE_CSS): example/styles.css
	cp example/styles.css $(EXAMPLE_CSS)

example.png: build-example $(EXAMPLE_CSS)
	@NODE_PATH=$(OUTPUT) node -e "require('Main').main();" > $(EXAMPLE_OUT)

	aha -s -f $(EXAMPLE_OUT) | awk '/head/{print "<link rel=\"stylesheet\" href=\"$(EXAMPLE_CSS)\" \>"}1' > $(EXAMPLE_HTML)
	phantomjs example/rasterize.js $(EXAMPLE_HTML) example.png 200 4
	convert example.png -trim example.png
