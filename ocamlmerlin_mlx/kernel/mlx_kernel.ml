module This = struct
  module Extension = Extension
  module Mreader = Mreader
  module Mreader_recover = Mreader_recover
  module Mreader_explain = Mreader_explain
  module Mreader_lexer = Mreader_lexer
  module Mreader_parser = Mreader_parser
end
include Merlin_kernel
include This
