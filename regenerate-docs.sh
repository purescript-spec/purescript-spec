#/bin/bash

set -eu

SRC='src/**/*.purs'
LIB='bower_components/purescript-*/src/**/*.purs'

function project_purs() {
  cd src
  find . | grep .purs | cut -c 3-
}

function docgens() {
  for x in $(project_purs)
  do
    local noext=${x%.*};
    echo --docgen ${noext//\//.}:docs/$noext.md
  done
}

psc-docs --format markdown "$SRC" "$LIB" $(docgens)