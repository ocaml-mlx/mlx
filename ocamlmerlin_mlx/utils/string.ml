(* mlx: stdlib String plus edit_distance (stdlib >= 5.4) for older host
   compilers. Byte-based restricted Damerau-Levenshtein; upstream is
   Uchar-based, but this only powers spellcheck suggestions. *)
include Stdlib.String

let edit_distance ?limit s0 s1 =
  let n0 = length s0 and n1 = length s1 in
  let cap d = match limit with Some l when d >= l -> l | _ -> d in
  if s0 = s1 then 0
  else begin
    let prev2 = Array.make (n1 + 1) 0 in
    let prev = Array.init (n1 + 1) (fun j -> j) in
    let cur = Array.make (n1 + 1) 0 in
    for i = 1 to n0 do
      cur.(0) <- i;
      for j = 1 to n1 do
        let cost = if get s0 (i - 1) = get s1 (j - 1) then 0 else 1 in
        let d =
          min
            (min (prev.(j) + 1) (cur.(j - 1) + 1))
            (prev.(j - 1) + cost)
        in
        let d =
          if
            i > 1 && j > 1
            && get s0 (i - 1) = get s1 (j - 2)
            && get s0 (i - 2) = get s1 (j - 1)
          then min d (prev2.(j - 2) + 1)
          else d
        in
        cur.(j) <- d
      done;
      Array.blit prev 0 prev2 0 (n1 + 1);
      Array.blit cur 0 prev 0 (n1 + 1)
    done;
    cap prev.(n1)
  end
