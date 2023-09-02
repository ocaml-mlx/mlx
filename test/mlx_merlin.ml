open Mlx_kernel

let parse_string filename str =
  let src = Msource.make str in
  let cfg = Mconfig.initial in
  let cfg =
    {
      cfg with
      Merlin_kernel.Mconfig.query = { cfg.query with filename };
      (* override this so we don't try to run any extensions *)
      merlin = { cfg.merlin with extension_to_reader = [] };
    }
  in
  Mreader.parse cfg (src, None)

let () =
  let str = In_channel.input_all stdin in
  let res = parse_string "*stdin*" str in
  let () = List.iter (fun exn -> raise exn) res.lexer_errors in
  let () = List.iter (fun exn -> raise exn) res.parser_errors in
  match res.parsetree with
  | `Implementation str ->
      Format.printf "%a@." Ocaml_parsing.Pprintast.structure str
  | `Interface str ->
      Format.printf "%a@." Ocaml_parsing.Pprintast.signature str
