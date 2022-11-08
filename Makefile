test:
	opam exec dune -- runtest

format:
	opam exec dune -- build @fmt --auto-promote || true

check-format:
	opam exec dune -- build @fmt

deps: _opam
	opam install dune ocamlformat ppx_show ppx_expect --yes

_opam:
	opam switch create . --package=ocaml-base-compiler.4.14.0
