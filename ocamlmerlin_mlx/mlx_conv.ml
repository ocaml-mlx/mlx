(* Converts the vendored parser's AST (merlin 505 / OCaml 5.5 shape)
   into the AST of the merlin-lib this binary is linked against (the
   host compiler's shape), via ppxlib's cross-version migrations.

   Some notes on usage of Obj.magic here...
   - mlx_ocaml_parsing's Parsetree is copied from merlin's 505 branch,
     which copies the AST from OCaml's 5.5.x branch, so it is
     structurally identical to Astlib.Ast_505 and the first magic is
     shape-preserving. TODO: instead we should inject ppxlib's Ast_505
     module there
   - Ppxlib_ast.Compiler_version is the AST of the host compiler, which
     is also the AST merlin-lib is compiled against, so the final magic
     is shape-preserving too. *)

module Conv =
  Ppxlib_ast.Convert
    (Ppxlib_ast__Versions.OCaml_505)
    (Ppxlib_ast.Compiler_version)

let conv_signature (intf : Mlx_ocaml_parsing.Parsetree.signature) :
    Ocaml_parsing.Parsetree.signature =
  let intf : Astlib.Ast_505.Parsetree.signature = Obj.magic intf in
  let intf = Conv.copy_signature intf in
  let intf : Ocaml_parsing.Parsetree.signature = Obj.magic intf in
  intf

let conv_structure (impl : Mlx_ocaml_parsing.Parsetree.structure) :
    Ocaml_parsing.Parsetree.structure =
  let impl : Astlib.Ast_505.Parsetree.structure = Obj.magic impl in
  let impl = Conv.copy_structure impl in
  let impl : Ocaml_parsing.Parsetree.structure = Obj.magic impl in
  impl
