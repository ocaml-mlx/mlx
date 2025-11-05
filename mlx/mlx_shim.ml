module Pprintast = struct
  include Pprintast

  let tyvar = Compiler_pprintast.tyvar
end

module Location = struct
  include Location

  let mkloc txt loc = { Location.txt; loc }
  let mknoloc txt = { Location.txt; loc = Location.none }
end

module Longident = struct
  include Longident

  let last = function
    | Lident s -> s
    | Ldot (_, s) -> s
    | Lapply (_, _) -> Misc.fatal_error "Longident.last"
end
