(* mlx: the parts of stdlib Pair (stdlib >= 5.4) the vendored merlin
   sources use, for older host compilers. *)
let map_fst f (a, b) = f a, b
let map_snd f (a, b) = a, f b
