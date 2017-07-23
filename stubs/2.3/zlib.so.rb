module Zlib
  BEST_COMPRESSION = 9
  OS_MSDOS = 0
  OS_WIN32 = 11
  SYNC_FLUSH = 2
  FILTERED = 1
  OS_VMS = 2
  BINARY = 0
  OS_CODE = 3
  HUFFMAN_ONLY = 2
  OS_TOPS20 = 10
  FINISH = 4
  DEFAULT_COMPRESSION = -1
  NO_COMPRESSION = 0
  OS_MACOS = 7
  MAX_MEM_LEVEL = 9
  OS_ZSYSTEM = 8
  OS_UNIX = 3
  OS_ATARI = 5
  DEF_MEM_LEVEL = 8
  ZLIB_VERSION = "1.2.3"
  class ZStream
    ##% adler : (*!FIXME) -> !FIXME
    def adler(*) end
    ##% avail_in : (*!FIXME) -> !FIXME
    def avail_in(*) end
    ##% avail_out : (*!FIXME) -> !FIXME
    def avail_out(*) end
    ##% avail_out= : (*!FIXME) -> !FIXME
    def avail_out=(*) end
    ##% close : (*!FIXME) -> !FIXME
    def close(*) end
    ##% closed? : (*!FIXME) -> !FIXME
    def closed?(*) end
    ##% data_type : (*!FIXME) -> !FIXME
    def data_type(*) end
    ##% end : (*!FIXME) -> !FIXME
    def end(*) end
    ##% ended? : (*!FIXME) -> !FIXME
    def ended?(*) end
    ##% finish : (*!FIXME) -> !FIXME
    def finish(*) end
    ##% finished? : (*!FIXME) -> !FIXME
    def finished?(*) end
    ##% flush_next_in : (*!FIXME) -> !FIXME
    def flush_next_in(*) end
    ##% flush_next_out : (*!FIXME) -> !FIXME
    def flush_next_out(*) end
    ##% reset : (*!FIXME) -> !FIXME
    def reset(*) end
    ##% stream_end? : (*!FIXME) -> !FIXME
    def stream_end?(*) end
    ##% total_in : (*!FIXME) -> !FIXME
    def total_in(*) end
    ##% total_out : (*!FIXME) -> !FIXME
    def total_out(*) end
  end

  OS_VMCMS = 4
  BEST_SPEED = 1
  OS_CPM = 9
  OS_RISCOS = 13
  VERSION = "2.3.3"
  ASCII = 1
  OS_AMIGA = 1
  MAX_WBITS = 15
  class Inflate < Zlib::ZStream
    ##% "<<" : (*!FIXME) -> !FIXME
    def <<(*) end
    ##% inflate : String -> String
    def inflate(s) end
    ##% initialize<t> : (?Fixnum) -> t
    def initialize(x=nil) end
    ##% set_dictionary : (*!FIXME) -> !FIXME
    def set_dictionary(*) end
    ##% sync : (*!FIXME) -> !FIXME
    def sync(*) end
    ##% sync_point? : (*!FIXME) -> !FIXME
    def sync_point?(*) end
    ##% Inflate.inflate : String -> String
    def Inflate.inflate(s) end
  end

  OS_QDOS = 12
  OS_UNKNOWN = 255
  class Error < StandardError
  end

  OS_OS2 = 6
  FULL_FLUSH = 3
  class BufError < Zlib::Error
  end

  DEFAULT_STRATEGY = 0
  NO_FLUSH = 0
  UNKNOWN = 2
  class GzipFile
    class Error < Zlib::Error
    end

    class CRCError < Zlib::GzipFile::Error
    end

    class LengthError < Zlib::GzipFile::Error
    end

    class NoFooter < Zlib::GzipFile::Error
    end

    ##% close : (*!FIXME) -> !FIXME
    def close(*) end
    ##% closed? : (*!FIXME) -> !FIXME
    def closed?(*) end
    ##% comment : (*!FIXME) -> !FIXME
    def comment(*) end
    ##% crc : (*!FIXME) -> !FIXME
    def crc(*) end
    ##% finish : (*!FIXME) -> !FIXME
    def finish(*) end
    ##% level : (*!FIXME) -> !FIXME
    def level(*) end
    ##% mtime : (*!FIXME) -> !FIXME
    def mtime(*) end
    ##% orig_name : (*!FIXME) -> !FIXME
    def orig_name(*) end
    ##% os_code : (*!FIXME) -> !FIXME
    def os_code(*) end
    ##% sync : (*!FIXME) -> !FIXME
    def sync(*) end
    ##% sync= : (*!FIXME) -> !FIXME
    def sync=(*) end
    ##% to_io : (*!FIXME) -> !FIXME
    def to_io(*) end
    ##% GzipFile.wrap : (*!FIXME) -> !FIXME
    def GzipFile.wrap(*rest) end
  end

  class StreamError < Zlib::Error
  end

  class DataError < Zlib::Error
  end

  class NeedDict < Zlib::Error
  end

  class GzipWriter < Zlib::GzipFile
    ##% "<<" : (*!FIXME) -> !FIXME
    def <<(*) end
    ##% comment= : (*!FIXME) -> !FIXME
    def comment=(*) end
    ##% flush : (*!FIXME) -> !FIXME
    def flush(*rest) end
    ##% initialize: ([write: ([to_s: () -> String]) -> Integer]) -> GzipWriter
    def initialize(io) end
    ##% mtime= : (*!FIXME) -> !FIXME
    def mtime=(*) end
    ##% orig_name= : (*!FIXME) -> !FIXME
    def orig_name=(*) end
    ##% pos : (*!FIXME) -> !FIXME
    def pos(*) end
    ##% print : (*!FIXME) -> !FIXME
    def print(*rest) end
    ##% printf : (*!FIXME) -> !FIXME
    def printf(*rest) end
    ##% putc : (*!FIXME) -> !FIXME
    def putc(*) end
    ##% puts : (*!FIXME) -> !FIXME
    def puts(*rest) end
    ##% tell : (*!FIXME) -> !FIXME
    def tell(*) end
    ##% write : String -> Fixnum
    def write(s) end
    ##% GzipWriter.open : (*!FIXME) -> !FIXME
    def GzipWriter.open(*rest) end
    ##% GzipWriter.wrap<t> : ([write: ([to_s: () -> String]) -> Integer]) {GzipWriter -> t} -> t
    def GzipWriter.wrap(*) end
  end

  class Deflate < Zlib::ZStream
    ##% "<<" : (*!FIXME) -> !FIXME
    def <<(*) end
    ##% deflate : (*!FIXME) -> !FIXME
    def deflate(*rest) end
    ##% flush : (*!FIXME) -> !FIXME
    def flush(*rest) end
    ##% params : (*!FIXME) -> !FIXME
    def params(*) end
    ##% set_dictionary : (*!FIXME) -> !FIXME
    def set_dictionary(*) end
    ##% Deflate.deflate : (String,?Fixnum) -> String
    def Deflate.deflate(str,level=nil) end
  end

  class VersionError < Zlib::Error
  end

  class StreamEnd < Zlib::Error
  end

  class GzipReader < Zlib::GzipFile
    include Enumerable
    ##% each : (*!FIXME) -> !FIXME
    def each(*rest) end
    ##% each_byte : (*!FIXME) -> !FIXME
    def each_byte(*) end
    ##% each_line : (*!FIXME) -> !FIXME
    def each_line(*rest) end
    ##% eof : (*!FIXME) -> !FIXME
    def eof(*) end
    ##% eof? : (*!FIXME) -> !FIXME
    def eof?(*) end
    ##% getc : (*!FIXME) -> !FIXME
    def getc(*) end
    ##% gets : (*!FIXME) -> !FIXME
    def gets(*rest) end
    ##% initialize: ([read : (?Fixnum, ?String) -> String]) -> GzipReader
    def initialize(io) end
    ##% lineno : (*!FIXME) -> !FIXME
    def lineno(*) end
    ##% lineno= : (*!FIXME) -> !FIXME
    def lineno=(*) end
    ##% pos : (*!FIXME) -> !FIXME
    def pos(*) end
    ##% read : (?Fixnum, ?String) -> String
    def read(*rest) end
    ##% readchar : (*!FIXME) -> !FIXME
    def readchar(*) end
    ##% readline : (*!FIXME) -> !FIXME
    def readline(*rest) end
    ##% readlines : (*!FIXME) -> !FIXME
    def readlines(*rest) end
    ##% rewind : (*!FIXME) -> !FIXME
    def rewind(*) end
    ##% tell : (*!FIXME) -> !FIXME
    def tell(*) end
    ##% ungetc : (*!FIXME) -> !FIXME
    def ungetc(*) end
    ##% unused : (*!FIXME) -> !FIXME
    def unused(*) end
    ##% GzipReader.open : (*!FIXME) -> !FIXME
    def GzipReader.open(*rest) end
  end

  class MemError < Zlib::Error
  end

  ##% Zlib.adler32 : (*!FIXME) -> !FIXME
  def Zlib.adler32(*rest) end
  ##% Zlib.crc32 : (*!FIXME) -> !FIXME
  def Zlib.crc32(*rest) end
  ##% Zlib.crc_table : (*!FIXME) -> !FIXME
  def Zlib.crc_table(*) end
  ##% Zlib.zlib_version : (*!FIXME) -> !FIXME
  def Zlib.zlib_version(*) end
end
