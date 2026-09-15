(* mlx: stdlib Map with the signature and functor extended by the list
   conversions (stdlib >= 5.1) that the vendored merlin 505 sources use,
   for older host compilers. Interfaces seal instances against Map.S, so
   S itself must carry the additions. *)
include
  (Stdlib.Map :
    module type of Stdlib.Map
      with module Make := Stdlib.Map.Make
       and module type S := Stdlib.Map.S)

module type S = sig
  include Stdlib.Map.S

  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val add_to_list : key -> 'a -> 'a list t -> 'a list t
end

module Make (Ord : Stdlib.Map.OrderedType) : S with type key = Ord.t = struct
  include Stdlib.Map.Make (Ord)

  let of_list l = List.fold_left (fun m (k, v) -> add k v m) empty l
  let to_list m = bindings m

  let add_to_list k v m =
    update k (function None -> Some [ v ] | Some l -> Some (v :: l)) m
end
