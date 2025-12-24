.PHONY: install
install:
	opam install . --deps-only -y
	opam install menhir.20201216 ocamlformat ocaml-lsp-server -y

.PHONY: create-switch
create-switch:
	opam switch create . 5.4.0 --no-install -y

.PHONY: init
init: create-switch install

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test
