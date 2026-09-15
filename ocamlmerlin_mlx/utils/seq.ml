(* mlx: stdlib Seq plus functions the vendored merlin 505 sources use
   that only exist on newer host compilers. Reached via -open Mlx_utils;
   on a 5.5 host the redefinitions shadow identical stdlib ones. *)
include Stdlib.Seq

let delay f () = f () ()
