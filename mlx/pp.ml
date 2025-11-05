let print_ml = ref false
let print_tokens = ref false
let input = ref None

let speclist =
  [
    "-print-ml", Arg.Set print_ml, "Print .ml syntax";
    "-print-tokens", Arg.Set print_tokens, "Print tokens";
  ]

module Conv =
  Ppxlib_ast.Convert
    (Ppxlib_ast__Versions.OCaml_501)
    (Ppxlib_ast.Compiler_version)

let () =
  Arg.parse speclist
    (fun input' -> input := Some input')
    "mlx-pp [-print-ml] [-print-tokens] <input-file>";
  let fname, src =
    match !input with
    | None -> "*stdin*", In_channel.input_all stdin
    | Some fname ->
        fname, In_channel.with_open_bin fname In_channel.input_all
  in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf fname;
  if !print_tokens then
    let rec loop () =
      match Lexer.token lexbuf with
      | Parser.EOF -> ()
      | _token ->
          let toks =
            String.sub src lexbuf.lex_start_p.pos_cnum
              (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_start_p.pos_cnum)
          in
          Format.printf "%s@." toks;
          loop ()
    in
    loop ()
  else
    let str =
      try Ok (Parse.implementation lexbuf)
      with exn -> (
        match Location.error_of_exn exn with
        | None -> raise exn
        | Some error -> Error error)
    in
    match str with
    | Ok str ->
        let str = Conv.copy_structure str in
        if !print_ml then
          Format.printf "%a@." Compiler_pprintast.structure str
        else
          let oc = stdout in
          output_string oc
            Ppxlib_ast.Compiler_version.Ast.Config.ast_impl_magic_number;
          output_value oc fname;
          output_value oc str
    | Error `Already_displayed -> exit 1
    | Error (`Ok error) ->
        Format.eprintf "%a@." Location.print_report error;
        exit 1
