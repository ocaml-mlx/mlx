(* mlx: stdlib Digest plus a BLAKE128 submodule (stdlib >= 5.2) for
   older host compilers. The vendored consistbl only needs the type;
   this binary never loads cmis through it, so backing it with MD5
   (also 128 bits) is inert. *)
include Stdlib.Digest

module BLAKE128 = struct
  type t = string

  let equal = Stdlib.String.equal
  let compare = Stdlib.compare
  let string = Stdlib.Digest.string
  let to_hex = Stdlib.Digest.to_hex
end
