module Pprintast = struct
  include Pprintast

  let tyvar = Compiler_pprintast.tyvar
  module Doc = Ocaml_common.Pprintast.Doc
end

module Location = struct
  include Location

  type error = Ppxlib.Location.Error.t

  let mkloc txt loc = { txt; loc }
  let mknoloc txt = { txt; loc = none }
  let map f { txt; loc } = { txt = f txt; loc }
  let input_name = Ocaml_common.Location.input_name
  let curr = Ocaml_common.Location.curr

  let error ?(loc = none) ?(sub = []) ?footnote:_ msg =
    Ppxlib.Location.Error.make ~loc ~sub msg

  let errorf ?loc ?sub ?footnote:_ fmt =
    Ocaml_common.Format_doc.kasprintf (error ?loc ?sub) fmt

  let msg ?(loc = none) fmt =
    Ocaml_common.Format_doc.kasprintf (fun msg -> loc, msg) fmt
  let deprecated = Ocaml_common.Location.deprecated
  let error_of_exn = Ocaml_common.Location.error_of_exn
  let print_report = Ocaml_common.Location.print_report
  let register_error_of_exn = Ppxlib.Location.Error.register_error_of_exn
  let prerr_warning = Ocaml_common.Location.prerr_warning
end

module Longident = struct
  include Longident

  let last = function
    | Lident s -> s
    | Ldot (_, s) -> s.txt
    | Lapply (_, _) -> Misc.fatal_error "Longident.last"

  let rec flatten_aux acc = function
    | Lident s -> s :: acc
    | Ldot (p, s) -> flatten_aux (s.txt :: acc) p.txt
    | Lapply (_, _) -> Misc.fatal_error "Longident.flatten"

  let flatten lid = flatten_aux [] lid
end
