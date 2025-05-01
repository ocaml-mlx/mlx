# Contributing

Thanks for your interest!

## Installation

**Prerequisites**:
- Install the [opam](https://opam.ocaml.org/) package manager

After cloning the repository, make sure to initialize git submodules:

```sh
$ git submodule update --init --recursive --remote
```

Run `make init` to set up an opam [local switch](https://opam.ocaml.org/blog/opam-local-switches/) and download the required dependencies.

## Build

```sh
make build
```
