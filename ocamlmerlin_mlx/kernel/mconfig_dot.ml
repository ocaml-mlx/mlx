open Std

let find_project_context _path = assert false
let get_config _ctx _path = assert false

type config = {
  build_path   : string list;
  source_path  : string list;
  hidden_build_path  : string list;
  hidden_source_path : string list;
  cmi_path     : string list;
  cmt_path     : string list;
  flags        : string list with_workdir list;
  extensions   : string list;
  suffixes     : (string * string) list;
  stdlib       : string option;
  reader       : string list;
  exclude_query_dir : bool;
  use_ppx_cache : bool;
}
