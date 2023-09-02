open Merlin_extend
open Merlin_extend.Extend_protocol.Reader
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

  let parse { text; path; _ } =
    let res = parse_string path text in
    match res.parsetree with
    | `Interface intf -> Signature intf
    | `Implementation impl -> Structure impl

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
