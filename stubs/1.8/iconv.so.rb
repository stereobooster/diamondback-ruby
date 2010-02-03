
class Iconv < Data

  ##% Iconv.charset_map : () -> Hash<String,String>
  def Iconv.charset_map() end

  ##% Iconv.conv : (String,String,String) -> String
  def Iconv.conv(to, from, str) end

  ##% Iconv.iconv : (String,String,*String) -> Array<String>
  def Iconv.iconv(to, from, *strs) end
  
  ##% initialize: (String,String) -> Iconv
  def initialize(to,from) end
  
  ##% Iconv.open: (String,String) -> Iconv
  ##% Iconv.open<t>: (String,String) {Iconv -> t} -> t
  def Iconv.open(to,from) end

  ##% close: () -> String
  def close() end

  # MikeF: The core-doc API has the completely wrong type for this method
  # I think its something like this, but we should double check by looking
  # at the C file
  ##% iconv: (String,?Fixnum,?Fixnum) -> String
  def iconv() end

  # TODO
  module Failure; end
  class BrokenLibrary < RuntimeError; end
  class IllegalSequence < ArgumentError; end
  class InvalidCharacter < ArgumentError; end
  class InvalidEncoding < ArgumentError; end
  class OutOfRange < RuntimeError; end

end


