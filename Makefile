
.PHONY: all build-deps doc sphinx odoc view fmt fmt-check install dev-deps test
DEV_DEPS := merlin ocamlformat

all:
	dune build
	cp -f _build/default/src/main.exe ez-cmdliner


build-deps:
	opam install --deps-only ./ez-cmdliner.opam

doc: sphinx odoc

html:
	sphinx-build sphinx docs/doc

odoc:
	dune build @doc
	rsync -auv --delete _build/default/_doc/_html/. docs/api

view:
	xdg-open file://$$(pwd)/docs/doc/index.html

fmt:
	dune build @fmt --auto-promote

fmt-check:
	dune build @fmt

install:
	dune install

dev-deps:
	opam install -y ${DEV_DEPS}

test:
	dune build @runtest
