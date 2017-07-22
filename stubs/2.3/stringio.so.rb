
##% StringIO <= BaseIO
class StringIO < Data
  include Enumerable

  ##% initialize<t>: (?String,?Fixnum) -> t
  def initialize(string="", mode=0) end

  ##% StringIO.open<t>: (?String,?Fixnum) {StringIO -> t} -> t
  def StringIO.open(string="", mode=0) end

  ##% "<<"<self> : [to_s : () -> String] -> self
  def <<(x) end

  ##% binmode<self> : () -> self
  def binmode() end

  ##% close: () -> NilClass
  def close() end

  ##% close_read: () -> NilClass
  def close_read() end

  ##% close_write:() -> NilClass
  def close_write() end

  ##% closed_read? : ()  -> Boolean
  def closed_read?() end

  ##% closed_write?:() -> Boolean
  def closed_write?() end

  ##% closed?: () -> Boolean
  def closed?() end

  ##% each<t,self>: (?String) {String -> t} -> self
  def each(*rest) end

  ##% each_byte<t,self> : () {Fixnum -> t} -> self
  def each_byte() end

  ##% each_line<t,self>: (?String) {String -> t} -> self
  def each_line(*rest) end

  ##% eof:() -> Boolean
  def eof() end

  ##% eof?:() -> Boolean
  def eof?() end

  # MikeF: Always raises NotImplementedError
  ##% fcntl : (Numeric, Numeric or String) -> Integer 
  def fcntl(*rest) end

  ##% fileno:() -> Fixnum
  def fileno() end

  ##% flush<self>:() -> self
  def flush() end

  ##% fsync:() -> Fixnum
  def fsync() end

  ##% getc:() -> Fixnum
  def getc() end

  ##% gets: (?String) -> String
  def gets(*rest) end

  ##% isatty:() -> Boolean
  def isatty () end

  ##% length:() -> Integer
  def length() end

  ##% lineno:() -> Integer
  def lineno() end

  ##% lineno= : Integer -> Integer
  def lineno=(p0) end

  ##% path:() -> NilClass
  def path() end

  ##% pid:() -> Fixnum
  def pid() end

  ##% pos:() -> Integer
  def pos() end

  ##% pos= : Integer -> Integer
  def pos=(p0) end

  ##% print : (*[to_s : () -> String]) -> NilClass
  def print(*rest) end

  ##% printf : (String, *[to_s : () -> String]) -> NilClass
  def printf(fmt, *rest) end

  ##% putc : Numeric -> Fixnum
  ##% putc : String -> String
  def putc(p0) end

  ##% puts : (*[to_s : () -> String]) -> NilClass
  def puts(*rest) end

  ##% read : (?Fixnum, ?String) -> String
  def read(*rest) end 

  ##% readchar:() -> Fixnum
  def readchar() end

  ##% readline: (?String) -> String
  def readline(*rest) end

  ##% readlines: (?String) -> Array<String>
  def readlines(*rest) end

  ##% reopen<self> : StringIO -> self
  ##% reopen<self> : (String,String) -> self
  def reopen(*rest) end

  ##% rewind:() -> Fixnum
  def rewind() end

  ##% seek : (Fixnum, Fixnum) -> Fixnum
  def seek(*rest) end

  ##% size: () -> Integer
  def size() end

  ##% string: () -> String
  def string() end

  ##% string= : (String) -> String
  def string=(p0) end 

  ## always returns true
  ##% sync:() -> Boolean 
  def sync() end

  ##% sync= : (Boolean) -> Boolean
  def sync=(p0) end

  ##% sysread:(?Fixnum, ?String) -> String
  def sysread(*rest) end

  ##% syswrite: ([to_s: () -> String]) -> Integer
  def syswrite(p0) end

  ##% tell:() -> Integer
  def tell() end

  ##% truncate: Integer -> Integer
  def truncate(p0) end

  ##% tty?: () -> Boolean
  def tty?() end 

  ##% ungetc: Integer -> NilClass
  def ungetc (p0) end

  ##% write: ([to_s: () -> String]) -> Integer
  def write(p0) end

end

