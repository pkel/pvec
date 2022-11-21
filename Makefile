test:
	opam exec dune -- runtest

format:
	opam exec dune -- build @fmt --auto-promote || true

check-format:
	opam exec dune -- build @fmt

deps: _opam dune-project
	opam exec dune -- build {pvec,_dev}.opam
	opam install . --deps-only --working-dir

_opam:
	opam switch create . --package=ocaml-base-compiler.4.14.0
	opam install . --deps-only --working-dir
