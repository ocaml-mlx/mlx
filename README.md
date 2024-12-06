# mlx

An OCaml syntax dialect which adds JSX expressions to the language.

```ocaml
let header ~title () = 
  <header>
    <h1>title</h1>
  </header>

let page =
  <html>
    <body>
      <header title="Hello, world!" />
      <div>
        "Some content goes here"
      </div>
    </body>
  </html>
```

This code is transformed into the following OCaml code:
```ocaml
let header ~title () =
  header () ~children:[ h1 () ~children:[ title ] [@JSX]; ] [@JSX]

let page =
  html () ~children:[
    body () ~children:[
      header () ~title:"Hello, world!" [@JSX];
      div () ~children:[ "Some content goes here" ] [@JSX];
    ] [@JSX];
  ] [@JSX]
```

It is expected to use `mlx-pp` preprocessor with either a runtime lib which
provides the implementation of such functions or a ppx which which further
transforms `[@JSX]` attributes into the desired output.

## Installation & Usage

Use the following commands to install the necessary packages:
```sh
opam install mlx ocamlmerlin-mlx
```

To make dune consider `.mlx` files as OCaml files you need to configure an mlx
dialect, put this in your `dune-project` file:
```lisp
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))
```

### Editor Support

#### VS Code
Syntax highlighting support for VS Code is still work in progress. As a temporary workaround, you can enable basic OCaml syntax highlighting by adding this to your settings:
```json
{
  "files.associations": {
    "*.mlx": "ocaml"
  }
}
```

#### Neovim
For Neovim users, install plugin: https://github.com/ocaml-mlx/ocaml_mlx.nvim

## Useful Links

- Example with Dream web framework: https://github.com/aantron/dream/pull/330
- Template ReasonReact project: https://github.com/andreypopp/melange-mlx-template
- mlx announcement on OCaml Discourse: https://discuss.ocaml.org/t/ann-mlx-syntax-dialect/15035
