(* mlx: stdlib Format plus utf_8_scalar_width (stdlib >= 5.4), needed by
   the vendored merlin format_doc on older host compilers. On a 5.5 host
   the redefinition shadows an identical stdlib one. *)
include Stdlib.Format

let utf_8_scalar_width ?(pos = 0) ?len s =
  let stop = match len with None -> String.length s | Some l -> pos + l in
  let rec go i acc =
    if i >= stop then acc
    else
      let c = Char.code s.[i] in
      let step =
        if c < 0x80 then 1 else if c < 0xE0 then 2 else if c < 0xF0 then 3
        else 4
      in
      go (i + step) (acc + 1)
  in
  go pos 0
