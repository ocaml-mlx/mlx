# mlx

An OCaml syntax dialect which adds JSX expressions to the language.

```
let header ~children () = 
  <header>
    <h1>title</h1>
  </header>

let page =
  <html>
    <body>
      <header>"Hello, world!"</header>
      <div>
        "Some content goes here"
      </div>
    </body>
  </html>
```

This code is transformed into the following OCaml code:
```
let header ~children () =
  header () ~children:[ h1 () ~children:[ title ] [@JSX]; ] [@JSX]

let page =
  html () ~children:[
    body () ~children:[
      header () ~children:[ "Hello, world!" ] [@JSX];
      div () ~children:[ "Some content goes here" ] [@JSX];
    ] [@JSX];
  ] [@JSX]
```

It is expected to use `mlx-pp` preprocessor with either a runtime lib which
provides the implementation of such functions or a ppx which which further
transforms `[@JSX]` attributes into the desired output.

## Installation & Usage

Currently for editor integration an unreleased version of `ocaml-lsp-server` is
needed (along with its depenedencies, `jsonrpc` and `lsp`).

Use the following commands to install the necessary packages:
```sh
opam pin add jsonrpc.dev --dev
opam pin add lsp.dev --dev
opam pin add ocaml-lsp-server.dev --dev
opam install mlx ocamlmerlin-mlx
```

To make dune consider `.mlx` files as OCaml files you need to configure an mlx
dialect, put this in your `dune-project` file:
```
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))
```
