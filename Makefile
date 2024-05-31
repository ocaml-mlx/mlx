.PHONY: init
init:
	opam switch create . 5.1.1 --no-install -y
	opam install . --deps-only -y
	opam install menhir.20201216 ocamlformat ocaml-lsp-server -y

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test
