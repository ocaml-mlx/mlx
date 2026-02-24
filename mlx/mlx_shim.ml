module Pprintast = struct
  include Pprintast

  let tyvar = Compiler_pprintast.tyvar
end

module Location = struct
  include Location

  type error = Ppxlib.Location.Error.t

  let mkloc txt loc = { txt; loc }
  let mknoloc txt = { txt; loc = none }
  let input_name = Ocaml_common.Location.input_name
  let curr = Ocaml_common.Location.curr

  let error ?(loc = none) ?(sub = []) ?footnote:_ msg =
    Ppxlib.Location.Error.make ~loc ~sub msg

  let errorf ?loc ?sub ?footnote:_ fmt =
    Format.kasprintf (error ?loc ?sub) fmt

  let msg ?(loc = none) fmt = Format.kasprintf (fun msg -> loc, msg) fmt
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
    | Ldot (_, { txt = s; _ }) -> s
    | Lapply (_, _) -> Misc.fatal_error "Longident.last"

  let rec flatten = function
    | Lident s -> [ s ]
    | Ldot ({ txt = lid; _ }, { txt = s; _ }) -> flatten lid @ [ s ]
    | Lapply _ -> Misc.fatal_error "Longident.flatten"

  let rec same a b =
    match a, b with
    | Lident a, Lident b -> String.equal a b
    | ( Ldot ({ txt = pa; _ }, { txt = a; _ }),
        Ldot ({ txt = pb; _ }, { txt = b; _ }) ) ->
        String.equal a b && same pa pb
    | Lapply _, _ | _, Lapply _ -> false
    | _ -> false
end
