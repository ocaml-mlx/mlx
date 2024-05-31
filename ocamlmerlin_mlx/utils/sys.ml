include Stdlib.Sys

let is_regular_file path =
  file_exists path && not (is_directory path)
