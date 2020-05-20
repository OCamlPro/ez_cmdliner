all: build

build:
	dune build

install:
	dune install

clean:
	rm -rf _build _obuild

ocp-build:
	ocp-build init
	ocp-build

