let () =
  let fname = Sys.argv.(1) in
  let src = In_channel.with_open_bin fname In_channel.input_all in
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
      let oc = stdout in
      output_string oc Config.ast_impl_magic_number;
      output_value oc fname;
      output_value oc str
  | Error `Already_displayed -> exit 1
  | Error (`Ok error) ->
      Format.eprintf "%a@." Location.print_report error;
      exit 1
