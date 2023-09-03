open Ocaml_parsing
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

let report_error exn =
  let () =
    match Location.error_of_exn exn with
    | Some (`Ok error) ->
        let ppf = Format.err_formatter in
        Location.print_loc ppf (Location.loc_of_report error);
        Format.pp_force_newline ppf ();
        Location.print_report ppf error;
        Format.pp_force_newline ppf ()
    | Some `Already_displayed -> ()
    | None -> raise exn
  in
  exit 1

let () =
  let str = In_channel.input_all stdin in
  let res = parse_string "*stdin*" str in
  let () = List.iter report_error res.lexer_errors in
  let () = List.iter report_error res.parser_errors in
  match res.parsetree with
  | `Implementation str ->
      Format.printf "%a@." Ocaml_parsing.Pprintast.structure str
  | `Interface str ->
      Format.printf "%a@." Ocaml_parsing.Pprintast.signature str
