# mlx

An OCaml syntax dialect which adds to JSX expressions to the language.

```
let header title = 
  <header>
    <h1>title</h1>
  </header>

let page =
  <html>
    <body>
      (header "Hello, world!")
      <div>
        "Some content gere"
      </div>
    </body>
  </html>
```

## Installation & Usage

While mlx is not yet available on opam, you can use a custom opam repository to install it:
```sh
opam repo add andreypopp https://github.com/andreypopp/opam-repository.git
opam update
opam pin add dune.dev --dev
opam pin add ocaml-lsp-server.dev --dev
opam install mlx ocamlmerlin-mlx ocamlformat-mlx
```

Then add the following config to your `dune-project` file:
```
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (format
   (run ocamlformat-mlx %{input-file}))
  (preprocess
   (run mlx-pp %{input-file}))))
```

Now dune will treat `.mlx` files as `.ml` files. If you use `ocaml-lsp-server`
then autocompletion and other editor integrations should work out of the box.

To format `.mlx` files you'd need to run `dune fmt` or configure
`ocamlformat-mlx` in your editor manually.
