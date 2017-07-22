
module Kernel
  ##% require : String -> Boolean
  def require(p0) end 
  ##% block_given? : () -> Boolean
  def block_given?() end
end

class Object
  ##% __splat<t> : Array<t> -> Array<t>
  ##% __splat<t> : t -> Array<t>
  def __splat(x) end
  ##% initialize<self> : () -> self
  def initialize() end
  ##% class : () -> Class 
  def class() end
  ##% extend<self> : (*Module) -> self
  def extend(*rest) end 
  ##% eval : (String, ?String, ?Fixnum) -> ?
  def eval(expr, *rest) end 
  ##% fail<t> : (?String) -> t
  ##% fail<t> : (Exception, ?String, ?Array) -> t
  def fail(*rest) end 
  ##% instance_eval : (String, ?String, ?Fixnum) -> ?
  ##% instance_eval<t> : () {() -> t} -> t
  def instance_eval(*rest) end 
  ##% proc<^t, u> : () {^t -> u} -> Proc<^t, u>
  def proc(&block) end
  ##% __send__ : (Symbol or String, *?) -> ?
  def __send__(*rest) end
  ##% send : (Symbol or String, *?) -> ?
  def send(*rest) end 
  ##% "=="<t> : t -> Boolean
  def ==(p0) end
end

class Exception; end

class Module
  def __im_module() false end

  ##% const_get : Symbol or String -> ?
  def const_get(p0) end
  ##% const_missing : Symbol or String -> Object
  def const_missing(p0) end
  ##% const_set : (String or Symbol, ?) -> ?
  def const_set(p0, p1) end
  ##% module_eval : (String, ?String, ?Fixnum) -> Object
  ##% module_eval<t> : () {() -> t} -> Object
  def module_eval(*rest) end 
end

class Class < Module
  def __im_class() false end
end

class NilClass
  def __im_nilclass() false end
end

class Numeric
  def __im_numeric() false end
  ##% "+" : Numeric -> Numeric
  def +(x) x end
  ##% "-" : Numeric -> Numeric
  def -(x) x end
end

class Integer < Numeric
  def __im_integer() false end
end

class Fixnum < Integer
  def __im_fixnum() false end
end

class Float < Numeric
  def __im_float() false end
end

class Boolean
  def __im_bool() false end
end

class String
  def __im_string() false end
  ##% "+" : String -> String
  def +(p0)  end
  ##% concat : String -> String
  def concat(x) x end
  ##% sub : (Regexp or String, String) -> String
  ##% sub : (Regexp or String) {String -> String} -> String
  def sub(pattern, *rest) end
end

##% Range<t>
class Range
  def __im_range() false end
end

##% Array<t>
class Array
  ##% "[]" : Range -> Array<t>
  ##% "[]" : (Fixnum, ?Array<Fixnum>) -> t
  def [](*args) end

  ##% at : Fixnum -> t
  def at(p0) end

  ##% each<u> : () {t -> u} -> Array<t>
  def each() end

  ##% empty?: () -> Boolean
  def empty?() end

  ##% to_a : () -> Array<t>
  def to_a() end

end

##% Hash<k,v>
class Hash
  ##% "[]" : k -> v
  def [](p0) end
  ##% "[]="<v'> ; v' <= v : (k, v') -> v' 
  def []=(p0, p1) end
end

##% Proc<^args,ret>
class Proc
  ##% call: (^args) -> ret
  def call(*) end
  ##% to_proc : () -> Proc<^args, ret>
  def to_proc() end
end

module DRuby
  class Profile
    class Runtime
      ##% Runtime.safe_require : (String,String) -> Boolean
      def self.safe_require(p1,p2) end
      ##% Runtime.safe_load : (String,String,?Boolean) -> Boolean
      def self.safe_load(*) end

      ##% Runtime.exn_require : (String) -> Boolean
      def self.exn_require(p1) end
      ##% Runtime.exn_load : (String,?Boolean) -> Boolean
      def self.exn_load(s,b=false) end
      
      ##% Runtime.dead_require : (*?) -> Boolean
      def self.dead_require(*) end
      
      class << self
        def dead_method(mname,*args) nil;nil end
      end
    end
  end
end
