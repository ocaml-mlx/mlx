(lang dune 3.15)

(using menhir 2.1)

(name mlx)

(generate_opam_files false)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (preprocess
   (run ./mlx/pp.exe %{input-file}))))

(source
 (github ocaml-mlx/mlx))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mlx))

(package
 (name ocamlmerlin-mlx))
