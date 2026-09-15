(* mlx: stdlib Sys plus is_regular_file (stdlib >= 5.1) for older host
   compilers. *)
include Stdlib.Sys

let is_regular_file path =
  try (Unix.stat path).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> raise (Sys_error (path ^ ": stat failed"))
