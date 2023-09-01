let print_ml = ref false
let input = ref None
let speclist = [ "-print-ml", Arg.Set print_ml, "Print .ml syntax" ]

let () =
  Arg.parse speclist
    (fun input' -> input := Some input')
    "mlx-pp [-print-ml] <input-file>";
  let fname, src =
    match !input with
    | None -> "*stdin*", In_channel.input_all stdin
    | Some fname ->
        fname, In_channel.with_open_bin fname In_channel.input_all
  in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf fname;
  let str =
    try Ok (Parse.implementation lexbuf)
    with exn -> (
      match Location.error_of_exn exn with
      | None -> raise exn
      | Some error -> Error error)
  in
  match str with
  | Ok str ->
      if !print_ml then Format.printf "%a@." Pprintast.structure str
      else
        let oc = stdout in
        output_string oc Config.ast_impl_magic_number;
        output_value oc fname;
        output_value oc str
  | Error `Already_displayed -> exit 1
  | Error (`Ok error) ->
      Format.eprintf "%a@." Location.print_report error;
      exit 1
