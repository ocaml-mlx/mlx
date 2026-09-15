(* mlx: stand-in for merlin-lib's Merlin_index_format, which older
   merlin-lib releases (4.x, for 4.14 hosts) don't ship. The vendored
   kernel only flushes its cache, and a reader never populates that
   cache, so a no-op is equivalent on every host. *)
module Index_cache = struct
  let flush ?older_than:_ () = ()
end
