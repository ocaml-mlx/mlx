(lang dune 3.9)

(using menhir 2.1)

(name mlx)

(generate_opam_files true)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (preprocess
   (run ./mlx/pp.exe %{input-file}))))

(source
 (github andreypopp/mlx))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mlx)
 (synopsis "OCaml syntax dialect which adds JSX syntax expressions")
 (depends
  ocaml
  dune
  cppo
  menhir
  csexp))
