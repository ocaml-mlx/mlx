(lang dune 3.9)

(using menhir 2.1)

(name mlx)

(generate_opam_files false)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (preprocess
   (run ./lib/pp.exe %{input-file}))))

(source
 (github andreypopp/mlx))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mlx))

(package
 (name ocamlmerlin-mlx)
 )
