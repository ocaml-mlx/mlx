include Ppxlib.Location

type error = Error.t

let mkloc txt loc = { txt; loc }
let mknoloc txt = { txt; loc = none }
let input_name = Ocaml_common.Location.input_name
let curr = Ocaml_common.Location.curr
let error ?(loc=none) ?(sub=[]) ?footnote:_ msg = Error.make ~loc ~sub msg
let errorf ?loc ?sub ?footnote:_ fmt = Format.kasprintf (error ?loc ?sub) fmt
let msg ?(loc=none) fmt = Format.kasprintf (fun msg -> loc, msg) fmt
let deprecated = Ocaml_common.Location.deprecated
let error_of_exn = Ocaml_common.Location.error_of_exn
let print_report = Ocaml_common.Location.print_report
let register_error_of_exn = Error.register_error_of_exn
let prerr_warning = Ocaml_common.Location.prerr_warning
