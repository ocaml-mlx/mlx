open Merlin_extend
open Merlin_extend.Extend_protocol.Reader
open Ocaml_parsing
open Mlx_preprocess
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

module Mlx_reader = struct
  type t = buffer

  let load buffer = buffer

  let mkstri exp =
    {
      Parsetree.pstr_loc = Location.none;
      pstr_desc = Pstr_eval (exp, []);
    }

  let mkexp ?(loc = Location.none) pexp_desc =
    {
      Parsetree.pexp_loc = loc;
      pexp_desc;
      pexp_attributes = [];
      pexp_loc_stack = [];
    }

  let mkpayload text =
    mkstri
      (mkexp
         (Pexp_constant
            (Parsetree.Pconst_string (text, Location.none, None))))

  let to_extension_node exn =
    match Location.error_of_exn exn with
    | None -> raise exn
    | Some `Already_displayed -> None
    | Some (`Ok error) ->
        let name =
          { Location.loc = error.main.loc; txt = "ocaml.error" }
        in
        let () = error.main.txt Format.str_formatter in
        let msg = Format.flush_str_formatter () in
        let payload = mkpayload msg in
        Some
          (mkstri
             (mkexp ~loc:error.main.loc
                (Parsetree.Pexp_extension
                   (name, Parsetree.PStr [ payload ]))))

  let parse { text; path; _ } =
    let res = parse_string path text in
    let parser_errors =
      List.filter_map to_extension_node res.parser_errors
    in
    let lexer_errors =
      List.filter_map to_extension_node res.lexer_errors
    in
    match res.parsetree with
    | `Interface intf -> Signature intf
    | `Implementation impl ->
        Structure (impl @ parser_errors @ lexer_errors)

  let for_completion t _pos = { complete_labels = true }, parse t

  let parse_line _ _ text =
    let res = parse_string "*buffer*" text in
    match res.parsetree with
    | `Interface intf -> Signature intf
    | `Implementation impl -> Structure impl

  let ident_at _ _ = []
  let pretty_print _ppf _ = ()

  let print_outcome ppf otree =
    Merlin_kernel.Mocaml.default_printer ppf otree
end

let () =
  let open Extend_main in
  let _ =
    match Sys.win32 with
    | true ->
        set_binary_mode_in stdin true;
        set_binary_mode_out stdout true
    | _ -> ()
  in
  extension_main
    ~reader:(Reader.make_v0 (module Mlx_reader : V0))
    (Description.make_v0 ~name:"mlx" ~version:"0.1")
