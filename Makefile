.PHONY: install
install:
	opam install . --deps-only -y

.PHONY: install-dev
install-dev:
	opam install . --deps-only --with-test --with-doc --with-dev-setup -y

.PHONY: create-switch
create-switch:
	opam switch create . 5.4.0 --no-install -y

.PHONY: init
init: create-switch install install-dev

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test
