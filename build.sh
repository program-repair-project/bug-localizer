#!/usr/bin/env bash

set -e

export OPAMYES=1

NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
OCAML_VERSION="4.11.0+flambda"
OPAM_SWITCH=program-repair-project-$OCAML_VERSION

opam init --compiler=$OCAML_VERSION -j $NCPU --no-setup

switch_exists=no
for installed_switch in $(opam switch list --short); do
  if [[ "$installed_switch" == "$OPAM_SWITCH" ]]; then
    switch_exists=yes
    break
  fi
done

if [ "$switch_exists" = "no" ]; then
  opam switch create $OPAM_SWITCH $OCAML_VERSION
else
  opam switch $OPAM_SWITCH
fi

eval $(SHELL=bash opam config env --switch=$SPARROW_OPAM_SWITCH)

opam pin add cil https://github.com/prosyslab/cil.git -n
opam install -j $NCPU dune batteries cil ppx_compare ocamlformat merlin yojson xmlm

make
