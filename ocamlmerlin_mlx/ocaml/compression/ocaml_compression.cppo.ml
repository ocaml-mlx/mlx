#if OCAML_VERSION < (5, 1, 0)
let input_value ic = Marshal.from_channel ic
let output_value oc v = Marshal.to_channel oc v []
#else
include Compression
#endif
