#!/bin/bash -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/setup_opam.sh

printf "Using ocaml %s from %s\n  and opam %s from %s\n" \
  "$(ocaml -vnum)" "$(which ocaml)" \
  "$(opam --version)" "$(which opam)"

printf "travis_fold:start:make\nBuilding diamondback-ruby\n"
omake
printf "travis_fold:end:make\n"
