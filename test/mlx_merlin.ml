open Mlx_ocaml_parsing
open Mlx_kernel

let parse_string filename str =
  let src = Msource.make str in
  let cfg = Mconfig.initial in
  let cfg =
    {
      cfg with
      Mconfig.query = { cfg.query with filename };
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
  (* -conv additionally exercises Mlx_conv (the Obj.magic + ppxlib
     migration bridge the reader uses to hand its AST to the host
     merlin): the converted AST is printed with the host merlin-lib's
     Pprintast, so a shape mismatch shows up as garbage or a crash. *)
  let conv = Array.mem "-conv" Sys.argv in
  let intf = Array.mem "-intf" Sys.argv in
  let str = In_channel.input_all stdin in
  let filename = if intf then "stdin.mli" else "*stdin*" in
  let res = parse_string filename str in
  let () = List.iter report_error res.lexer_errors in
  let () = List.iter report_error res.parser_errors in
  match res.parsetree with
  | `Implementation str ->
      if conv then
        Format.printf "%a@." Ocaml_parsing.Pprintast.structure
          (Mlx_conv.conv_structure str)
      else Format.printf "%a@." Mlx_ocaml_parsing.Pprintast.structure str
  | `Interface str ->
      if conv then
        Format.printf "%a@." Ocaml_parsing.Pprintast.signature
          (Mlx_conv.conv_signature str)
      else Format.printf "%a@." Mlx_ocaml_parsing.Pprintast.signature str
