test: _opam
	opam exec dune -- runtest

format: _opam
	opam exec dune -- build @fmt --auto-promote

check-format: _opam
	opam exec dune -- build @fmt

deps: _opam dune-project
	opam exec dune -- build {pvec,_dev}.opam
	opam install . --deps-only --with-test --working-dir

_opam:
	opam switch create . --package=ocaml-base-compiler.4.08.1 --deps-only --with-test
