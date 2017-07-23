# DRuby base_types.rb file

class String;end
class Integer;end
class IO;end

module Kernel
  ##% __backtick : String -> String
  def __backtick(p0) end
  ##% __splat<t> : Array<t> -> Array<t>
  ##% __splat<t> : t -> Array<t>
  def __splat(x) end

  # ##% Array<t> : [to_ary : () -> Array<t>] or [to_a : () -> Array<t>] -> \
  # ##%            Array<t>
  # ##% Array<t> : t -> Array<t> ; t !<= [to_ary : () -> Array<t>], \
  # ##%            t !<= [to_a : () -> Array<t>]
  # XXX: want to the above, but not as of now
  ##% Array<t> : [to_a : () -> Array<t>] -> Array<t>
  ##% Array<t> : [to_ary : () -> Array<t>] -> Array<t>
  ##% Array<t> : t -> Array<t>
  def Array(p0) end
  ##% Float : [to_f : () -> Float] -> Float
  def Float(p0) end
  ##% Integer : [to_i : () -> Fixnum] or [to_int : () -> Integer] -> Integer
  def Integer(p0) end
  ##% String : [to_s : () -> String] -> String
  def String(p0) end
  ##% abort : String or Fixnum -> NilClass
  ##% abort : () -> NilClass
  def abort(*rest) end
  ##% at_exit<^t, u> : () {^t -> u} -> Proc<^t,u> # DAVIDAN
  def at_exit(&block) end
  ##% autoload : (String or Symbol, String) -> NilClass
  def autoload(p0, p1) end
  ##% autoload? : (String or Symbol) -> String
  def autoload?(p0) end
  ##% binding : () -> Binding
  def binding() end
  ##% block_given? : () -> Boolean
  def block_given?() end
  ##% callcc<t> : () {Continuation -> t} -> t
  def callcc() end
  ##% caller : (?Fixnum) -> Array<String>
  def caller(start=1) end
  ##% catch<t> : (Symbol) {() -> t} -> t
  def catch(p0) end
  ##% chomp : (?String) -> String
  def chomp(sep=$/) end
  ##% chomp! : (?String) -> String
  def chomp!(sep=$/) end
  ##% chop : () -> String
  def chop() end # nil never returned
  ##% chop! : () -> String
  def chop!() end
  ##% eval : (String, ?String, ?Fixnum) -> ?
  ##% eval : (String, Binding, ?String, ?Fixnum) -> ?
  def eval(expr, *rest) end
  ##% exec : (String, *String) -> NilClass
  def exec(cmd, *rest) end
  ##% exit : (?Integer) -> NilClass
  ##% exit : (?Boolean) -> NilClass
  def exit(res=0) end
  ##% exit! : (?Fixnum) -> NilClass
  def exit!(res=0) end # no exit handler called
  ##% fail<t> : (?String) -> t
  ##% fail<t> : (Exception, ?String, ?Array) -> t
  ##% fail<t> : ([exception : () -> Exception], ?String, ?Array) -> t
  def fail(*rest) end
  ##% fork : () {() -> []} -> Fixnum
  def fork() end
  ##% format : (String, *[to_s : () -> String]) -> String
  def format(fmt, *rest) end
  ### This is obsolete
  # def getc() end
  ##% gets : (?String) -> String
  def gets(sep=$/) end
  ##% global_variables : () -> Array<String>
  def global_variables() end
  ##% gsub : (Regexp or String, String) -> String
  ##% gsub : (Regexp or String) {String -> String} -> String
  def gsub(*rest, &x) end
  ##% gsub! : (Regexp or String, String) -> String
  ##% gsub! : (Regexp or String) {String -> String} -> String
  def gsub!(*rest, &x) end
  ##% iterator? : () -> Boolean
  def iterator?() end
  ##% lambda<^t, u> : () {^t -> u} -> Proc<^t, u>
  def lambda(&block) end
  ##% load : (String, ?Boolean) -> Boolean
  def load(filename, wrap=false) end
  ##% local_variables : () -> Array<String>
  def local_variables() end
  ##% loop<t> : () {() -> t} -> t
  def loop() end
  ##% method_missing : (Symbol, *?) -> ?
  def method_missing(*rest) end
  # #% open<t,^rest> : ([open: (^rest) -> t]) -> t
  # #% open : ([to_str : () -> String], ?Integer, ?Integer) -> BaseIO
  # #% open<t> : ([to_str : () -> String], ?Integer, ?Integer) {BaseIO -> t} -> t
  ##% open : (*?) -> ?
  def open(path, *rest) end
  ##% p : (*[inspect : () -> String]) -> NilClass
  def p(*rest) end
  ##% print : (*[to_s : () -> String]) -> NilClass
  def print(*rest) end
  ##% printf : (BaseIO, String, *[to_s : () -> String]) -> NilClass
  ##% printf : (String, *[to_s : () -> String]) -> NilClass
  def printf(fmt, *rest) end
  ##% proc<^t, u> : () {^t -> u} -> Proc<^t, u>
  def proc(&block) end
  ##% putc : Integer -> Integer
  def putc(p0) end
  ##% puts : (*[to_s : () -> String]) -> NilClass
  ##% puts : (Array<[to_s : () -> String]>) -> NilClass
  def puts(*rest) end
  ##% raise<u> : (?String) -> u
  ##% raise<u> : ([exception : () -> Exception], ?String, ?Array) -> u
  def raise(*rest) end
  ##% rand : (?Numeric) -> Fixnum
  def rand(max=0) end
  ##% readline : (?String) -> String
  def readline(sep=$/) end
  ##% readlines : (?String) -> Array<String>
  def readlines(sep=$/) end
  ##% require : String -> Boolean
  def require(p0) end
  ##% scan : Regexp or String -> Array
  ##% scan : (Regexp or String) {(*String) -> []} -> String
  def scan(p0, &block) end
  ## Technically, this returns a 3-tuple of arrays
  ##% select : (Array<BaseIO>, ?Array<BaseIO>, ?Array<BaseIO>, ?Fixnum) -> Array<Array<BaseIO>>
  def select(*rest) end
  ##% set_trace_func<^t,u> : Proc<^t, u> -> Proc<^t, u>
  ##% set_trace_func : NilClass -> NilClass
  def set_trace_func(p0) end
  ##% sleep : (?Numeric) -> Fixnum
  def sleep(dur=0) end
  ##% split : (?String, ?Fixnum) -> Array<String>
  ##% split : (Regexp, ?Fixnum) -> Array<String>
  def split(pattern=$;, limit=0) end
  ##% sprintf : (String, *[to_s : () -> String]) -> String
  def sprintf(fmt, *rest) end
  ##% srand : (?Numeric) -> Integer
  def srand(num=0) end
  ##% sub : (Regexp or String, String) -> String
  ##% sub : (Regexp or String) {String -> String} -> String
  def sub(pattern, *rest, &x) end
  ##% sub! : (Regexp or String, String) -> String
  ##% sub! : (Regexp or String) {String -> String} -> String
  def sub!(pattern, *rest, &x) end
  ##% syscall : (Fixnum, *(String or Integer)) -> Integer
  def syscall(fixnum, *rest) end
  ##% system : ([to_str:() -> String], *[to_str:()->String]) -> Boolean
  def system(cmd, *rest) end
  ##% test : (Integer, String, ?String) -> Time or Boolean or Integer
  def test(cmd, file1, file2="foo") end
  ## 2nd arg here is the return value of the catch block
  ##% throw<t> : (Symbol, ?t) -> NilClass
  def throw(symb, *rest) end
  ##% trace_var : (Symbol, String or Proc) -> NilClass
  ##% trace_var<t> : (Symbol) {Object -> t} -> NilClass
  def trace_var(*rest) end
  ##% trap : (String, Proc) -> Object
  ##% trap<t> : (String) {() -> t} -> Object
  def trap(*rest) end
  ##% untrace_var : (Symbol, ?String) -> Array
  def untrace_var(*rest) end
  ##% warn : String -> NilClass
  def warn(p0) end
end

class Object
  ##% "=="<t> : t -> Boolean
  def ==(p0) end
  ##% "==="<t> : t -> Boolean
  def ===(p0) end
  ##% "=~"<t> : t -> Boolean # Object -> Boolean
  def =~(p0) end
  ##% __id__ : () -> Fixnum
  def __id__() end
  ##% __send__ : (Symbol or String, *?) -> ?
  def __send__(*rest) end
  ##% class : () -> Class
  def class() end
  ##% clone<self> : () -> self
  def clone() end
  ##% display : (?BaseIO) -> NilClass
  def display(port=$>) end
  ##% dup<self> : () -> self
  def dup() end
  ##% eql?<t> : t -> Boolean
  def eql?(p0) end
  ##% equal?<t> : t -> Boolean
  def equal?(p0) end
  ##% extend<self> : (*Module) -> self
  def extend(*rest) end
  ##% freeze<self> : () -> self
  def freeze() end
  ##% frozen? : () -> Boolean
  def frozen?() end
  ##% hash : () -> Fixnum
  def hash() end
  ##% id : () -> Fixnum
  def id() end
  ##% initialize : () -> []
  def initialize() end
  ##% inspect : () -> String
  def inspect() end
  ##% instance_eval : (String, ?String, ?Fixnum) -> ?
  ##% instance_eval<t> : () {() -> t} -> t
  def instance_eval(*rest) end
  ##% instance_of? : Module -> Boolean
  def instance_of?(p0) end
  ##% instance_variable_defined? : (Symbol or String) -> Boolean
  def instance_variable_defined?(p0) end
  ##% instance_variable_get : (Symbol or String) -> ?
  def instance_variable_get(p0) end
  ##% instance_variable_set : (Symbol or String, ?) -> ?
  def instance_variable_set(p0, p1) end
  ##% instance_variables : () -> Array
  def instance_variables() end
  ##% is_a? : Module -> Boolean
  def is_a?(p0) end
  ##% kind_of? : Module -> Boolean
  def kind_of?(p0) end
  ##% method : (Symbol or String) -> Method
  def method(p0) end
  ##% methods : () -> Array<String>
  def methods() end
  ##% nil? : () -> Boolean
  def nil?() end
  ##% object_id : () -> Fixnum
  def object_id() end
  ##% private_methods : (?Boolean) -> Array<String>
  def private_methods(all=true) end
  ##% protected_methods : (?Boolean) -> Array<String>
  def protected_methods(all=true) end
  ##% public_methods : (?Boolean) -> Array<String>
  def public_methods(all=true) end
  ##% respond_to? : (Symbol or String, ?Boolean) -> Boolean
  def respond_to?(symbol, include_private=false) end
  ##% send : (Symbol or String, *?) -> ?
  def send(*rest) end
  ##% singleton_methods : (?Boolean) -> Array<String>
  def singleton_methods(all=true) end
  ##% taint<self> : () -> self
  def taint() end
  ##% tainted? : () -> Boolean
  def tainted?() end

  ## MikeF: This method is deprecated (Ruby gives a runtime warning),
  ## so I suggest we not include it for now.  Also, it is important that
  ## Ruby classes implement this themselves whenever they are used in for
  ## loops or parallel assignment
  # #% to_a : () -> Array<Object>
  # def to_a() end

  ##% to_s : () -> String
  def to_s() end
  ##% type : () -> Class
  def type() end
  ##% untaint<self> : () -> self
  def untaint() end
end

module ObjectSpace # FIXME
  ##% ObjectSpace._id2ref : !FIXME -> !FIXME
  def ObjectSpace._id2ref(*) end
  ##% ObjectSpace.add_finalizer : !FIXME -> !FIXME
  def ObjectSpace.add_finalizer(*) end
  ##% ObjectSpace.call_finalizer : !FIXME -> !FIXME
  def ObjectSpace.call_finalizer(*) end

  ##% ObjectSpace.define_finalizer<t,v> : (t, Proc<^(t),v>) -> !FIXME
  def ObjectSpace.define_finalizer(*rest) end

  # DAVIDAN: a bit weird
  ##% ObjectSpace.each_object<t, u>: (?Module) {t -> u} -> Fixnum
  def ObjectSpace.each_object(*p) end

  ##% ObjectSpace.finalizers : !FIXME -> !FIXME
  def ObjectSpace.finalizers(*) end
  ##% ObjectSpace.garbage_collect : !FIXME -> !FIXME
  def ObjectSpace.garbage_collect(*) end
  ##% ObjectSpace.remove_finalizer : !FIXME -> !FIXME
  def ObjectSpace.remove_finalizer(*) end
  ##% ObjectSpace.undefine_finalizer<t> : t -> t
  def ObjectSpace.undefine_finalizer(p0) end

end

class Exception # FIXME
  ##% backtrace : () -> Array<String>
  def backtrace() end
  ##% exception<self> : (?[to_s : () -> String]) -> self
  def exception(*rest) end

  ##% initialize : (?String) -> []
  def initialize(p0="") end

  ##% inspect : () -> String
  def inspect() end

  ##% message : () -> String
  def message() end

  ##% set_backtrace : !FIXME -> !FIXME
  def set_backtrace(*) end
  ##% to_s : () -> String
  def to_s() end
  ##% to_str : () -> String
  def to_str() end
  ##% Exception.exception<self> : (?[to_s : () -> String]) -> self
  def Exception.exception(*rest) end
end

class MatchData # FIXME
  ##% "[]" : Fixnum or Symbol -> String
  ##% "[]" : (Fixnum, Fixnum) -> Array<String>
  ##% "[]" : Range -> Array<String>
  def [](*rest) end

  ##% begin : Fixnum -> Fixnum
  def begin(p0) end

  ##% captures: () -> Array<String>
  def captures() end

  ##% end : Fixnum -> Fixnum
  def end(p0) end
  ##% inspect : !FIXME -> !FIXME
  def inspect(*) end
  ##% length : !FIXME -> !FIXME
  def length(*) end
  ##% offset : !FIXME -> !FIXME
  def offset(*) end
  ##% post_match : () -> String
  def post_match() end
  ##% pre_match : () -> String
  def pre_match() end
  ##% select : !FIXME -> !FIXME
  def select(*) end
  ##% size : !FIXME -> !FIXME
  def size(*) end
  ##% string : !FIXME -> !FIXME
  def string(*) end
  ##% to_a : () -> Array<String>
  def to_a() end
  ##% to_s : () -> String
  def to_s() end
  ##% values_at : !FIXME -> !FIXME
  def values_at(*) end
end

MatchingData = MatchData

##% Thread<ret>
class Thread # FIXME
  ##% "[]" : (String or Symbol) -> ?
  def [](p0) end
  ##% "[]=" : (String or Symbol,?) -> ?
  def []=(p0, p1) end
  ##% abort_on_exception : !FIXME -> !FIXME
  def abort_on_exception(*) end
  ##% abort_on_exception= : !FIXME -> !FIXME
  def abort_on_exception=(*) end
  ##% alive? : () -> Boolean
  def alive?() end
  ##% exit : () -> Thread
  def exit() end
  ##% exit! : () -> Thread
  def exit!() end
  ##% group : !FIXME -> !FIXME
  def group(*) end
  ##% initialize<^args> : (^args) {(^args) -> ret} -> []
  def initialize(*rest) end
  ##% inspect : !FIXME -> !FIXME
  def inspect(*) end
  ##% join : !FIXME -> !FIXME
  def join(*) end
  ##% key? : !FIXME -> !FIXME
  def key?(*) end
  ##% keys : !FIXME -> !FIXME
  def keys(*) end
  ##% kill : () -> Thread
  def kill() end
  ##% kill! : () -> Thread
  def kill!() end
  ##% priority : !FIXME -> !FIXME
  def priority(*) end
  ##% priority= : !FIXME -> !FIXME
  def priority=(*) end
  ### Same as Kernel#raise
  ##% raise<u> : (?String) -> u
  ##% raise<u> : ([exception : () -> Exception], ?String, ?Array) -> u
  def raise(*rest) end
  ##% run : () -> Thread
  def run() end
  ##% safe_level : !FIXME -> !FIXME
  def safe_level(*) end
  ##% status : !FIXME -> !FIXME
  def status(*) end
  ##% stop? : !FIXME -> !FIXME
  def stop?(*) end
  ##% terminate : () -> Thread
  def terminate() end
  ##% terminate! : () -> Thread
  def terminate!() end
  ##% value : () -> ret
  def value() end
  ##% wakeup : () -> Thread
  def wakeup() end

  ##% Thread.abort_on_exception : () -> Boolean
  def Thread.abort_on_exception() end
  ##% Thread.abort_on_exception= : Boolean -> Boolean
  def Thread.abort_on_exception=(p0) end

  ##% Thread.critical: () -> Boolean
  def Thread.critical() end
  ##% Thread.critical=: (Boolean) -> Boolean
  def Thread.critical=(v) end

  ##% Thread.current: () -> Thread
  def Thread.current() end

  ##% Thread.exit: () -> Thread
  def Thread.exit() end

  ##% Thread.fork<^args> : (^args) {(^args) -> ret} -> Thread
  def Thread.fork(*rest) end
  ##% Thread.kill : Thread -> Thread
  def Thread.kill(p0) end
  ##% Thread.list : !FIXME -> !FIXME
  def Thread.list(*) end
  ##% Thread.main : !FIXME -> !FIXME
  def Thread.main(*) end
  ##% Thread.pass : () -> NilClass
  def Thread.pass() end
  ##% Thread.start<^args> : (^args) {(^args) -> ret} -> Thread
  def Thread.start(*rest) end
  ##% Thread.stop : () -> NilClass
  def Thread.stop() end
end

class ThreadGroup
  Default = ThreadGroup.new
  ##% add : Thread -> NilClass
  def add(p0) end
  ##% enclose : !FIXME -> !FIXME
  def enclose(*) end
  ##% enclosed? : !FIXME -> !FIXME
  def enclosed?(*) end
  ##% list : !FIXME -> !FIXME
  def list(*) end
end

class Module
  ##% Module.constants : () -> Array<String>
  def Module.constants() end
  ##% Module.nesting : () -> Array<Module>
  def Module.nesting() end
  ##% "<" : Module -> Boolean
  def <(p0) end
  ##% "<=" : Module -> Boolean
  def <=(p0) end
  ##% "<=>" : Module -> Fixnum
  def <=>(p0) end
  ##% "=="<t> : t -> Boolean
  def ==(p0) end
  ##% "==="<t> : t -> Boolean
  def ===(p0) end
  ##% ">" : Module -> Boolean
  def >(p0) end
  ##% ">=" : Module -> Boolean
  def >=(p0) end
  ##% alias_method : (Symbol, Symbol) -> Module
  def alias_method(sym1,sym2) end
  ##% append_features : Module -> Module
  def append_features(mod) end
  ##% attr : (Symbol or String, ?Boolean) -> NilClass
  def attr(symbol, writable=false) end
  ##% attr_accessor : (Symbol or String, *Symbol) -> NilClass
  def attr_accessor(sym, *more) end
  ##% attr_reader : (Symbol or String, *(Symbol or String)) -> NilClass
  def attr_reader(sym, *more) end
  ##% attr_writer : (Symbol or String, *Symbol) -> NilClass
  def attr_writer(sym, *more) end
  ##% ancestors : () -> Array<Module>
  def ancestors() end
  ##% autoload : (String or Symbol, String) -> NilClass
  def autoload(p0, p1) end
  ##% autoload? : String or Symbol -> String
  def autoload?(p0) end
  ##% class_eval : (String, ?String, ?Fixnum) -> Object
  ##% class_eval<t> : () {() -> t} -> Object
  def class_eval(*rest) end
  ##% class_variable_defined? : Symbol or String -> Boolean
  def class_variable_defined?(sym) end
  ##% class_variable_get : Symbol or String -> ?
  def class_variable_get(sym) end
  ##% class_variable_set : (Symbol or String, ?) -> ?
  def class_variable_set(sym,val) end
  ##% class_variables : () -> Array<String>
  def class_variables() end
  ##% const_defined? : Symbol or String -> Boolean
  def const_defined?(p0) end
  ##% const_get : Symbol or String -> ?
  def const_get(p0) end
  ##% const_missing : Symbol or String -> Object
  def const_missing(p0) end
  ##% const_set : (String or Symbol, ?) -> ?
  def const_set(p0, p1) end
  ##% constants : () -> Array<String>
  def constants() end
  ##% define_method : (String or Symbol, Method) -> Method
  ##% define_method<^args,ret> : (String or Symbol) {^args -> ret} -> Method<^args,ret>
  def define_method(sym,*meth) end
  ##% extend_object<t> : t -> t
  def extend_object(obj) end
  ### XXX: not documented
  ##% extended : !FIXME -> !FIXME
  def extended(*) end
  ##% freeze<self> : () -> self
  def freeze() end
  ##% include : (Module, *Module) -> Module
  def include(mod,*more) end
  ##% included<t> : Module -> t
  def included(othermod) end
  ##% include? : Module -> Boolean
  def include?(p0) end
  ##% included_modules : () -> Array<Module>
  def included_modules() end
  ##% instance_method : (Symbol or String) -> UnboundMethod
  def instance_method(p0) end
  ##% instance_methods : (?Boolean) -> Array<String>
  def instance_methods(include_super=true) end
  ### XXX: not documented
  ##% method_added : !FIXME -> !FIXME
  def method_added(*) end
  ##% method_defined? : Symbol or String -> Boolean
  def method_defined?(p0) end
  ### XXX: not documented
  ##% method_removed : !FIXME -> !FIXME
  def method_removed(*) end
  ### XXX: not documented
  ##% method_undefined : !FIXME -> !FIXME
  def method_undefined(*) end
  ##% module_eval : (String, ?String, ?Fixnum) -> Object
  ##% module_eval<t> : () {() -> t} -> Object
  def module_eval(*rest) end
  ##% module_function : (*Symbol) -> Module
  def module_function(sym, *rest) end
  ##% name : () -> String
  def name() end
  ##% nesting : () -> Array<Module>
  def nesting() end
  ##% private : *(Symbol or String) -> Module
  def private(*syms) end
  ##% private_class_method : *(Symbol or String) -> Module
  def private_class_method(*rest) end
  ##% private_instance_methods : (?Boolean) -> Array<String>
  def private_instance_methods(include_super=true) end
  ##% private_method_defined? : Symbol or String -> Boolean
  def private_method_defined?(p0) end
  ##% protected : *(Symbol or String) -> Module
  def protected(*syms) end
  ##% protected_instance_methods : (?Boolean) -> Array<String>
  def protected_instance_methods(include_super=true) end
  ##% protected_method_defined? : Symbol or String -> Boolean
  def protected_method_defined?(p0) end
  ##% public : *(Symbol or String) -> Module
  def public(*syms) end
  ##% public_class_method : *(Symbol or String) -> Module
  def public_class_method(*rest) end
  ##% public_instance_methods : (?Boolean) -> Array<String>
  def public_instance_methods(include_super=true) end
  ##% public_method_defined? : Symbol or String -> Boolean
  def public_method_defined?(p0) end
  ##% remove_class_variable : Symbol or String -> Module
  def remove_class_variable(sym) end
  ##% remove_const : Symbol or String -> Module
  def remove_const(sym) end
  ##% remove_method : Symbol or String -> Module
  def remove_method(sym) end
  ##% to_s : () -> String
  def to_s() end
  ##% undef_method : Symbol or String -> Module
  def undef_method (sym) end
end

class Class < Module ### DONE
  ##% allocate<self>: () -> self
  def allocate() end
  # # MikeF: Optional, dynamic arg (the superclass)
  ##% initialize : (??) -> []
  def initialize(*)
  end
  ##% superclass: () -> Class
  def superclass() end
end

module Precision
  ##% Precision.included<t> : Class -> t
  def Precision.included(p0) end  # magical mixin crud
  ### XXX: proc : Class -> Objproc : Class -> Object
  ##% prec : !FIXME -> !FIXME
  def prec(*) end  # seems to always fail...
  ##% prec_f : () -> Float
  def prec_f() end
  ##% prec_i : () -> Fixnum
  def prec_i() end
end


module Comparable
  ##% "<"<t> : t -> Boolean
  def <(p0) end
  ##% "<="<t> : t -> Boolean
  def <=(p0) end
  ##% "=="<t> : t -> Boolean
  def ==(p0) end
  ##% ">"<t> : t -> Boolean
  def >(p0) end
  ##% ">="<t> : t -> Boolean
  def >=(p0) end
  ##% between?<t> : (t, t) -> Boolean
  def between?(p0, p1) end
end

class Numeric
  include Precision
  ##% "%"<self> : Numeric -> self
  def %(p0) end
  ##% "&"<self> : Numeric -> self
  def &(p0) end
  ##% "*"<self> : Numeric -> self
  def *(p0) end
  ##% "**"<self> : Numeric -> self
  def **(p0) end
  ##% "+"<self> : Numeric -> self
  def +(p0) end
  ##% "+@"<self> : () -> self
  def +@() end
  ##% "-"<self> : Numeric -> self
  def -(p0) end
  ##% "-@"<self> : () -> self
  def -@() end
  ##% "/"<self> : Numeric -> self
  def /(p0) end
  ##% "<" : Numeric -> Boolean
  def <(p0) end
  ##% "<<"<self> : Numeric -> self
  def <<(p0) end
  ##% "<=" : Numeric -> Boolean
  def <=(p0) end
  ##% "<=>" : Numeric -> Fixnum
  def <=>(p0) end
  ##% "=="<t> : t -> Boolean # Numeric -> Boolean
  def ==(p0) end
  ##% ">" : Numeric -> Boolean
  def >(p0) end
  ##% ">=" : Numeric -> Boolean
  def >=(p0) end
  ##% ">>"<self> : Numeric -> self
  def >>(p0) end
  ##% "[]" : Numeric -> Fixnum
  def [](p0) end
  ##% "^"<self> : Numeric -> self
  def ^(p0) end
  ##% abs<self> : () -> self
  def abs() end
  ##% between?: (Numeric, Numeric) -> Boolean
  def between?(x,y) end
  ##% ceil<self> : () -> self
  def ceil() end
  ##% chr : () -> String
  def chr() end
  ##% coerce : Numeric -> Array<Numeric>
  def coerce(p0) end
  ##% denominator : () -> Fixnum
  def denominator() end
  ##% div<self> : Numeric -> self
  def div(p0) end
  ##% divmod<self> : Numeric -> Array<self>
  def divmod(p0) end
  ##% downto<t,self> : (Numeric) {Numeric -> t} -> self
  def downto(p0) end
  ##% eql?<t> : t -> Boolean #Numeric -> Boolean
  def eql?(p0) end
  ##% finite? : () -> Boolean
  def finite?() end
  ##% floor<self> : () -> self
  def floor() end
  ##% gcd : Numeric -> Numeric
  def gcd(p0) end
  ##% gcdlcm : Numeric -> Array<Numeric>
  def gcdlcm(p0) end
  ##% hash : () -> Fixnum
  def hash() end
  ##% id2name : () -> Fixnum
  def id2name() end
  ##% infinite? : () -> Boolean
  def infinite?() end
  ##% integer? : () -> Boolean
  def integer?() end
  ##% lcm : Numeric -> Numeric
  def lcm(p0) end
  ##% modulo<self> : Numeric -> self
  def modulo(p0) end
  ##% nan? : () -> Boolean
  def nan?() end
  ##% next<self> : () -> self
  def next() end
  ##% nonzero? : () -> Boolean
  def nonzero?() end
  ##% numerator : () -> Fixnum
  def numerator() end
  ##% power!<self> : Numeric -> self
  def power!(p0) end
  ##% quo : Numeric -> Float
  def quo(n) end
  ##% rdiv : Numeric -> Float
  def rdiv(p0) end
  ##% remainder<self> : Numeric -> self
  def remainder(p0) end
  ##% round<self> : () -> self
  def round() end
  ##% rpower<self> : Numeric -> self
  def rpower(n) end
  ##% singleton_method_added : !FIXME -> !FIXME
  def singleton_method_added(*) end
  ##% size : () -> Fixnum
  def size() end # Had better be a fixnum!
  ##% step<t,self> : (Numeric, Numeric) {Numeric -> t} -> self
  def step(limit, step) end
  ##% succ<self> : () -> self
  def succ() end
  ##% times<t> : () {Integer -> t} -> Integer
  def times() end
  ##% to_f : () -> Float
  def to_f() end
  ##% to_i : () -> Fixnum
  def to_i() end
  ##% to_r : () -> Rational
  def to_r() end
  ##% to_int : () -> Integer
  def to_int() end
  ##% to_sym : () -> Symbol
  def to_sym() end
  ##% truncate : () -> Fixnum
  def truncate() end
  ##% upto<t> : (Integer) {Integer -> t} -> Integer
  def upto(p0) end
  ##% zero? : () -> Boolean
  def zero?() end
  ##% "|"<self> : Numeric -> self
  def |(p0) end
  ##% "~"<self> : () -> self
  def ~() end
end

class Rational; end

##% Integer <= Numeric
class Integer < Numeric ### DONE
  ##% Integer.induced_from : Numeric -> Integer
  def Integer.induced_from(p0) end
end

##% Bignum <= Integer
class Bignum < Integer ### DONE
  ##% Bignum.induced_from : Numeric -> Bignum
  def Bignum.induced_from(p0) end
end

##% Fixnum <= Integer
class Fixnum < Integer ### DONE
  ##% Fixnum.induced_from : Numeric -> Fixnum
  def Fixnum.induced_from(p0) end

  ##% to_s : ?Fixnum -> String
  def to_s(x=10) end
end

class Float < Numeric ### DONE
  DIG = 15
  EPSILON = 2.22044604925031e-16
  MANT_DIG = 53
  MAX = 1.79769313486232e+308
  MAX_10_EXP = 308
  MAX_EXP = 1024
  MIN = 2.2250738585072e-308
  MIN_10_EXP = -307
  MIN_EXP = -1021
  RADIX = 2
  ROUNDS = 1

  ##% Float.induced_from : Numeric -> Float
  def Float.induced_from(p0) end
end

class Symbol ### DONE
  ### XXX: documentation is confusing (=== or ==) ?
  ##% "===" : !FIXME -> !FIXME
  def ===(*) end
  ##% Symbol.all_symbols : () -> Array<Symbol>
  def Symbol.all_symbols() end
  ##% id2name : () -> String
  def id2name() end
  ##% inspect : () -> String
  def inspect() end
  ##% to_i : () -> Fixnum
  def to_i() end
  ##% to_int : () -> Fixnum
  def to_int() end
  ##% to_s : () -> String
  def to_s() end
  ##% to_sym : () -> Symbol
  def to_sym() end
end

class NilClass ### DONE
  ##% "&"<t> : t -> Boolean
  def &(p0) end
  ##% "^"<t> : t -> Boolean
  def ^(p0) end
  ##% inspect : () -> String
  def inspect() end
  ##% nil? : () -> Boolean
  def nil?() end
  ##% to_a : () -> Array # empty array
  def to_a() end
  ##% to_f : () -> Float
  def to_f() end
  ##% to_i : () -> Fixnum
  def to_i() end
  ##% to_s : () -> String
  def to_s() end
  ##% "|"<t> : t -> Boolean
  def |(p0) end
end

class Regexp ### DONE
  EXTENDED = 2
  IGNORECASE = 1
  MULTILINE = 4
  ##% "=="<t> : t -> Boolean # : Regexp -> Boolean
  def ==(p0) end
  ##% "===" : String -> Boolean
  def ===(p0) end
  ##% "=~" : [to_str : () -> String] -> Fixnum
  def =~(p0) end
  ##% casefold? : () -> Boolean
  def casefold?() end
  ##% eql?<t> : t -> Boolean #Regexp -> Boolean
  def eql?(p0) end
  ##% hash : () -> Fixnum
  def hash() end
  ##% initialize : String -> []
  ##% initialize : Regexp -> []
  ##% initialize : (String, [], ?String) -> []
  def initialize(p0, *rest) end
  ##% inspect : () -> String
  def inspect() end
  ##% kcode : () -> String
  def kcode() end
  ##% match : String -> MatchData
  def match(p0) end
  ##% options : () -> Fixnum
  def options() end
  ##% source : () -> String
  def source() end
  ##% to_s : () -> String
  def to_s() end
  ##% "~" : () -> Integer
  def ~() end
  ##% Regexp.compile : Regexp or String -> Regexp
  ##% Regexp.compile : (String, String) -> Regexp
  def Regexp.compile(p0, *rest) end
  ##% Regexp.escape : String -> String
  def Regexp.escape(p0) end
  ##% Regexp.last_match : () -> MatchData
  ##% Regexp.last_match : Fixnum -> String
  def Regexp.last_match(*rest) end
  ##% Regexp.quote : String -> String
  def Regexp.quote(p0) end
  ##% Regexp.union : *(String or Regexp) -> Regexp
  def Regexp.union(*rest) end
end

class Boolean
  ##% "&" : Boolean -> Boolean
  def &(p0) end
  ##% "^" : Boolean -> Boolean
  def ^(p0) end
  ##% to_s : () -> String
  def to_s() end
  ##% "|" : Boolean -> Boolean
  def |(p0) end
end

class TrueClass < Boolean ### DONE
  ##% "&" : TrueClass -> Boolean
  def &(p0) end
  ##% "^" : TrueClass -> Boolean
  def ^(p0) end
  ##% to_s : () -> String
  def to_s() end
  ##% "|" : TrueClass -> Boolean
  def |(p0) end
end

##% Enumerable<t>
module Enumerable
  # Mixed in with classes with an each method that returns contents
  ##% all?<v> : () {t -> v} -> Boolean
  def all?() end
  ##% any?<v> : () {t -> v} -> Boolean
  def any?() end
  ##% collect<u> : () {t -> u} -> Array<u>
  def collect() end
  ##% detect<v> : () {t -> v} -> t
  def detect() end
  ##% each_with_index<u> : () {(t, Fixnum) -> u } -> Enumerable<t>
  def each_with_index() end
  ##% entries : () -> Array<t>
  def entries() end
  ##% find<v>: () {t -> v} -> t
  def find() end
  ##% find_all<v> : () {t -> v} -> Array<t>
  def find_all() end
  ##% grep : Regexp -> Array<t>
  ##% grep<u> : (Regexp) {t -> u} -> Array<u>
  def grep(p0) end
  ##% include? : t -> Boolean
  def include?(p0) end
  ##% inject<self,v,u> ; self <= [each: () {v -> []} -> []] : (u) {(u, v) -> u} -> u
  def inject(*rest) end
  ##% map<u> : () {t -> u} -> Array<u>
  def map() end
  # #% max<self,u> ; self <= [each: () {["<=>" : t -> Fixnum] -> u} -> Enumerable<t>] \
  # #% : () -> t
  ##% max : () -> t
  ##% max : () {(t, t) -> Fixnum} -> t
  def max() end
  ##% member? : t -> Boolean
  def member?(p0) end
  # #% min<self,u> ; self <= [each: () {["<=>" : t -> Fixnum] -> u} -> Enumerable<t>] \
  # #% : () -> t
  ##% min : () -> t
  ##% min : () {(t, t) -> Fixnum} -> t
  def min() end
  ##% partition<v> : () {t -> v} -> Array<Array<t> >
  def partition() end
  ##% reject<v> : () {t -> v} -> Array<t>
  def reject() end
  ##% select<v> : () {t -> v} -> Array<t>
  def select() end
  # #% sort<self,u> ; self <= [each: () {["<=>" : t -> Fixnum] -> u} -> Enumerable<t>] \
  # #% : () -> Array<t>
  ##% sort : () {(t, t) -> Fixnum} -> Array<t>
  ##% sort : () -> Array<t>
  def sort() end
  ##% sort_by<u> ; u <= ["<=>" : t -> Fixnum] : () {t -> u} -> Array<t>
  def sort_by() end
  ##% to_a : () -> Array<t>
  def to_a() end
  ##% zip : Array<Array<t> > -> Array<Array<t> >
  ##% zip<u> : (Array<Array<t> >) {Array<t> -> u} -> NilClass
  def zip(*rest) end
end

class FalseClass < Boolean
  ##% "&" : FalseClass -> Boolean
  def &(p0) end
  ##% "^" : FalseClass -> Boolean
  def ^(p0) end
  ##% to_s : () -> String
  def to_s() end
  ##% "|" : FalseClass -> Boolean
  def |(p0) end
end

##% Hash<k, v>
class Hash
  include Enumerable
  ##% "=="<t> : t -> Boolean
  def ==(p0) end
  ##% "[]" : k -> v
  def [](p0) end
  ##% "[]="<v'> ; v' <= v : (k, v') -> v'
  def []=(p0, p1) end
  ##% clear : () -> Hash<k, v>
  def clear() end
  ##% collect<u> : () {(k,v) -> u} -> Array<u>
  def collect() end
  ##% default : (?k) -> v
  def default(key) end
  ##% default= : v -> Hash<k, v>
  def default=(p0) end
  # # should be (hash<k,v>,k) -> v
  ##% default_proc : () -> Proc<^(Hash<k, v>, k),v>
  def default_proc() end
  ##% delete : k -> v
  ##% delete<u> : (k) {k -> u} -> u or v
  def delete(p0) end
  ##% delete_if<t> : () {(k, v) -> t} -> Hash<k, v>
  def delete_if() end
  ##% each<u> : () {(k, v) -> u} -> Hash<k, v>
  def each() end
  ##% each_key<u> : () {k -> u} -> Hash<k, v>
  def each_key() end
  ##% each_pair<u> : () {(k, v) -> u} -> Hash<k, v>
  def each_pair() end
  ##% each_value<u> : () {v -> u} -> Hash<k, v>
  def each_value() end
  ##% empty? : () -> Boolean
  def empty?() end
  ##% fetch : k -> v
  ##% fetch<u> : (k, u) -> v or u
  ##% fetch<u> : (k) {k -> u} -> v or u
  def fetch(key, *rest) end
  ##% has_key? : k -> Boolean
  def has_key?(p0) end
  ##% has_value? : v -> Boolean
  def has_value?(p0) end
  ##% include? : k -> Boolean
  def include?(p0) end
  ##% index : v -> k
  def index(p0) end
  ##% indexes : (*k) -> Array<Array<k or v> >
  def indexes(*rest) end
  ##% indices : (*k) -> Array<Array<k or v> >
  def indices(*rest) end
  ##% initialize : (?v) -> []
  ##% initialize : () {(Hash<k, v>, k) -> v} -> []
  def initialize(*rest, &block) end
  ##% inspect : () -> String
  def inspect() end
  ##% invert : () -> Hash<v, k>
  def invert() end
  ##% key? : k -> Boolean
  def key?(p0) end
  ##% keys : () -> Array<k>
  def keys() end
  ##% length : () -> Fixnum
  def length() end
  ##% member? : k -> Boolean
  def member?(p0) end
  ### NOTE: this function has different type annotation than merge!
  ###       because it does not manipulate the contents of Hash object.
  ###       Thus, we can provide more precise type info. for return type.
  ##% merge<k', v'> ; k' <= k, v' <= v: Hash<k', v'> -> Hash<k' or k, v' or v>
  ##% merge<k', v', u> ; k' <= k, v' <= v : (Hash<k', v'>) {(k, v, v') -> u} -> \
  ##%                    Hash<k, v or v' or u>
  def merge(p0) end
  ##% merge! : Hash<k, v> -> Hash<k, v>
  ##% merge! : (Hash<k, v>) {(k, v, v) -> v} -> Hash<k, v>
  def merge!(p0) end
  ##% rehash : () -> Hash<k, v>
  def rehash() end
  ##% reject<t> : () {(k, v) -> t} -> Hash<k, v>
  def reject() end
  ##% reject!<t> : () {(k, v) -> t} -> Hash<k, v>
  def reject!() end
  ##% replace<k', v'> ; k' <= k, v' <= v : Hash<k', v'> -> Hash<k', v'>
  def replace(p0) end
  ##% select<t> : () {(k, v) -> t} -> Hash<k, v>
  def select() end
  ##% shift : () -> (k, v)
  def shift() end
  ##% size : () -> Fixnum
  def size() end
  ##% sort<self> ; self <= [each<u>: () {["<=>" : k -> Fixnum] -> u} -> Hash<k,v>] \
  ##%            : () -> Array<Array<k or v> >
  ##% sort : () {(Array<k or v>, Array<k or v>) -> Fixnum} -> \
  ##%        Array<Array<k or v>>
  def sort() end
  ##% store<k', v'> ; k' <= k, v' <= v : (k', v') -> v'
  def store(p0, p1) end
  ##% to_a : () -> Array<Array<Object> > # k or v> >
  def to_a() end
  ##% to_hash : () -> Hash<k, v>
  def to_hash() end
  ##% to_s : () -> String
  def to_s() end
  ##% update : Hash<k, v> -> Hash<k, v>
  ##% update : (Hash<k, v>) {(k, v, v) -> v} -> Hash<k, v>
  def update(p0) end
  ##% value? : v -> Boolean
  def value?(p0) end
  ##% values : () -> Array<v>
  def values() end
  ##% values_at : (*k) -> Array<v>
  def values_at(*rest) end
  ### MF: this is a little imprecise for the latter case since it
  ### requires alternating key/value pairs in the argument list
  ##% Hash."[]"<k',v'> : Hash<k',v'> -> Hash<k',v'>
  ##% Hash."[]"<k',v',t> ; t <= k', t <= v' : (k',v', *t) -> Hash<k',v'>
  def Hash.[](*rest) end
  ##% to_yaml: () -> String
  def to_yaml() end
end

class String
  # The ! variants of fns return nil if no modification was made
  include Comparable
  include Enumerable
  ##% "%" : [to_s: () -> String] -> String
  ##% "%" : Array<[to_s : () -> String]>-> String
  def %(p0) end
  ##% "*" : Integer -> String
  def *(p0) end
  ##% "+" : [to_str:()->String] -> String
  def +(p0) end
  ##% "<<" : Fixnum -> String
  ##% "<<" : [to_s : () -> String] -> String
  def <<(p0) end
  ##% "<=>" : String -> Fixnum
  def <=>(p0) end
  ##% "=="<t> : t -> Boolean
  def ==(p0) end
  ##% "=~" : ["=~" : String -> Fixnum] -> Fixnum
  def =~(p0) end
  ##% "[]" : Fixnum -> Fixnum
  ##% "[]" : Range or Regexp or String -> String
  ##% "[]" : (Fixnum or Regexp, Fixnum) -> String
  def [](p0, *rest) end
  ##% "[]=" : (Fixnum, Fixnum) -> Fixnum
  ##% "[]=" : (Fixnum, String) -> String
  ##% "[]=" : (Fixnum, Fixnum, String) -> String
  ##% "[]=" : (Range or Regexp or String, String) -> String
  def []=(p0, *rest) end
  ##% capitalize : () -> String
  def capitalize() end
  ##% capitalize! : () -> String
  def capitalize!() end
  ##% casecmp : String -> Fixnum
  def casecmp(p0) end
  ##% center : (Integer, ?String) -> String
  def center(p0, padstr="") end
  ##% chomp : (?String) -> String
  def chomp(sep=$/) end
  ##% chomp! : (?String) -> String
  def chomp!(sep=$/) end
  ##% chop! : () -> String
  def chop!() end
  ##% chop : () -> String
  def chop() end
  ##% concat : Fixnum -> String
  ##% concat : [to_str : () -> String] -> String
  def concat(p0) end
  ##% count : (String, *String) -> Fixnum
  def count(*rest) end
  ##% crypt : String -> String
  def crypt(p0) end
  ##% delete : (String, *String) -> String
  def delete(*rest) end
  ##% delete! : (String, *String) -> String
  def delete!(*rest) end
  ##% downcase : () -> String
  def downcase() end
  ##% downcase! : () -> String
  def downcase!() end
  ##% dump : () -> String
  def dump() end
  ##% each<t> : (?String) {String -> t} -> String
  def each(sep=$/) end
  ##% each_byte<t> : () {Fixnum -> t} -> String
  def each_byte() end
  ##% each_line<t> : (?String) {String -> t} -> String
  def each_line(sep=$/) end
  ##% empty? : () -> Boolean
  def empty?() end
  ##% eql?<t> : t -> Boolean #String -> Boolean
  def eql?(p0) end
  ##% gsub : (Regexp or String, String) -> String
  ##% gsub : (Regexp or String) {String -> String} -> String
  def gsub(pattern, *rest) end
  ##% gsub! : (Regexp or String, String) -> String
  ##% gsub! : (Regexp or String) {String -> String} -> String
  def gsub!(pattern, *rest) end
  ##% hash : () -> Fixnum
  def hash() end
  ##% hex : () -> Integer
  def hex() end
  ##% include? : Fixnum or String -> Boolean
  def include?(p0) end
  ##% index : (String or Fixnum or Regexp, ?Fixnum) -> Fixnum
  def index(loc, offset=0) end
  ##% initialize : (?[to_str: () -> String]) -> []
  def initialize(z="") end
  ##% insert : (Fixnum, String) -> String
  def insert(p0, p1) end
  ##% inspect : () -> String
  def inspect() end
  ##% intern : () -> String
  def intern() end
  ##% length : () -> Fixnum
  def length() end
  ##% ljust : (Integer, ?String) -> String
  def ljust(p0, padstr="") end
  ##% lstrip : () -> String
  def lstrip() end
  ##% lstrip! : () -> String
  def lstrip!() end
  ##% match : (Regexp or String) -> MatchData
  def match(p0) end # todo
  ##% next : () -> String
  def next() end
  ##% next! : () -> String
  def next!() end
  ##% oct : () -> Integer
  def oct() end
  ##% replace : String -> String
  def replace(p0) end
  ##% reverse : () -> String
  def reverse() end
  ##% reverse! : () -> String
  def reverse!() end
  ##% rindex : (String or Fixnum or Regexp, ?Fixnum) -> Fixnum
  def rindex(loc, stop=99) end
  ##% rjust : (Integer, ?String) -> String
  def rjust(p0, padstr="") end
  ##% rstrip : () -> String
  def rstrip() end
  ##% rstrip! : () -> String
  def rstrip!() end
  ##% scan : Regexp or String -> Array
  ##% scan<t> : (Regexp or String) {(*String) -> t} -> String
  def scan(p0) end
  ##% size : () -> Integer
  def size() end
  ##% slice : Fixnum -> Fixnum
  ##% slice : (Fixnum or Regexp, Fixnum) -> String
  ##% slice : Range or Regexp or String -> String
  def slice(p0, *rest) end
  ##% slice! : Fixnum -> Fixnum
  ##% slice! : (Fixnum or Regexp, Fixnum) -> String
  ##% slice! : Range or Regexp or String -> String
  def slice!(p0, *rest) end
  ##% split : (?String, ?Fixnum) -> Array<String>
  ##% split : (Regexp, ?Fixnum) -> Array<String>
  def split(pattern=$;, limit=0) end
  ##% squeeze : String -> String
  def squeeze(*rest) end
  ##% squeeze! : String -> String # one argument (verified)
  def squeeze!(*rest) end
  ##% strip : () -> String
  def strip() end
  ##% strip! : () -> String
  def strip!() end
  ##% sub : (Regexp or String, String) -> String
  ##% sub : (Regexp or String) {String -> String} -> String
  def sub(pattern, *rest) end
  ##% sub! : (Regexp or String, String) -> String
  ##% sub! : (Regexp or String) {String -> String} -> String
  def sub!(pattern, *rest) end
  ##% succ : () -> String
  def succ() end
  ##% succ! : () -> String
  def succ!() end
  ##% sum : (?Fixnum) -> Integer
  def sum(n=16) end
  ##% swapcase : () -> String
  def swapcase() end
  ##% swapcase! : () -> String
  def swapcase!() end
  ##% to_f : () -> Float
  def to_f() end
  ##% to_i : (?Fixnum) -> Fixnum
  def to_i(base=10) end
  ##% to_s : () -> String
  def to_s() end
  ##% to_str : () -> String
  def to_str() end
  ##% to_sym : () -> Symbol
  def to_sym() end
  ##% tr : (String, String) -> String
  def tr(p0, p1) end
  ##% tr! : (String, String) -> String
  def tr!(p0, p1) end
  ##% tr_s : (String, String) -> String
  def tr_s(p0, p1) end
  ##% tr_s! : (String, String) -> String
  def tr_s!(p0, p1) end
  ##% unpack : String -> ?
  def unpack(p0) end
  ##% upcase : () -> String
  def upcase() end
  ##% upcase! : () -> String
  def upcase!() end
  ##% upto<t> : (String) {String -> t} -> String
  def upto(p0) end
end

##% Range<t> # t is required to instantiate Enumerable
class Range
  ### include Enumerable<t>
  include Enumerable
  # #% "==" : Range<t> -> Boolean
  ##% "==" : !FIXME -> !FIXME
  def ==(*) end
  ##% "===" : t -> Boolean
  def ===(p0) end
  ##% begin : () -> t
  def begin() end
  ##% each<u> : () {t -> u} -> Range<t>
  def each() end
  ##% end : () -> t
  def end() end
  ##% eql?<v> : v -> Boolean # Range<t> -> Boolean
  def eql?(p0) end
  ##% exclude_end? : () -> Boolean
  def exclude_end?() end
  ##% first : () -> t
  def first() end
  ##% hash : () -> Fixnum
  def hash() end
  ##% include? : t -> Boolean
  def include?(p0) end
  ##% initialize : (t, t, ?Boolean) -> []
  def initialize(a, b) end
  ##% inspect : () -> String
  def inspect() end
  ##% last : () -> t
  def last() end
  ##% member? : t -> Boolean
  def member?(p0) end
  ##% step<u> : (?Fixnum) {t -> u} -> Range
  def step(n=1) end
  ##% to_s : () -> String
  def to_s() end
end

##% Array<t>
class Array ### DONE
  include Enumerable
  ##% "&"<u> : Array<u> -> Array<t> # XXX: should I constrain u <= t?
  def &(p0) end
  ##% "*" : Integer -> Array<t>
  ##% "*" : String -> String
  def *(p0) end
  ##% "+"<u> : [to_ary:() -> Array<u>] -> Array<t or u>
  def +(p0) end
  ##% "-"<u> : Array<u> -> Array<t> # still t!
  def -(p0) end
  ##% "<<" <u> : u -> Array<t or u>
  def <<(p0) end
  ##% "<=>" <u> : Array<u> -> Fixnum
  def <=>(p0) end
  ##% "=="<v> : v -> Boolean
  def ==(p0) end
  ##% "[]" : Range -> Array<t>
  ##% "[]" : (Fixnum, Fixnum) -> Array<t> # DAVIDAN
  ##% "[]" : Fixnum -> t # DAVIDAN
  def [](p0, *rest) end
  ##% "[]=" <u> ; u <= t: (Fixnum, u) -> u
  ##% "[]=" <u> ; u <= t: (Fixnum, Fixnum, u) -> u
  ##% "[]=" <u> ; u <= t: (Fixnum, Fixnum, Array<u>) -> Array<u>
  ##% "[]=" <u> ; u <= t: (Range, u) -> u
  ##% "[]=" <u> ; u <= t: (Range, Array<u>) -> Array<u>
  def []=(p0, *rest) end
  ##% assoc<self,u> ; self <= Array<Array<u>> : u -> Array<u>
  def assoc(p0) nil end
  ##% at : Fixnum -> t
  def at(p0) end
  ##% clear : () -> Array<t> # XXX: or is it just Array?
  def clear() end
  ##% collect<u> : () {t -> u} -> Array<u>
  def collect() end
  ##% collect!<u> ; u <= t : () {t -> u} -> Array<u>
  def collect!() end
  ##% compact : () -> Array<t>
  def compact() end
  ##% compact! : () -> Array<t>
  def compact!() end
  ##% concat : Array<t> -> Array<t>
  def concat(p0) end
  ##% delete : t -> t
  ##% delete<u> : (t) {() -> u} -> t or u
  def delete(p0) end
  ##% delete_at : Fixnum -> t
  def delete_at(p0) end
  ##% delete_if<any> : () {t -> any} -> Array<t>
  def delete_if() end
  ##% each<u> : () {t -> u} -> Array<t>
  def each() end
  ##% each_index<u> : () {Fixnum -> u} -> Array<t>
  def each_index() end
  ##% empty? : () -> Boolean
  def empty?() end
  ##% eql?<v> : v -> Boolean #Array<t> -> Boolean
  def eql?(p0) end
  ##% fetch : Fixnum -> t
  ##% fetch<u> : (Fixnum, u) -> t or u
  ##% fetch<u> : (Fixnum) {Fixnum -> u} -> u
  def fetch(p0, *rest) end
  ##% fill<u> ; u <= t : (u, ?Fixnum, ?Fixnum) -> Array<t>
  ##% fill<u> ; u <= t : (u, Range) -> Array<t>
  ##% fill<u> ; u <= t : () {Fixnum -> u} -> Array<u> # notice Array<u>
  ##% fill<u> ; u <= t : (Fixnum, ?Fixnum) {Fixnum -> u} -> Array<t>
  ##% fill<u> ; u <= t : (Range) {Fixnum -> u} -> Array<t>
  def fill(*rest) end
  ##% first : () -> t
  ##% first : Fixnum -> Array<t>
  def first(*n) end
  ##% flatten : () -> Array # no error but not precise
  def flatten() end  # can't give this function a real type!
  ##% flatten! : () -> Array # XXX: throws type error since t is invalid
  def flatten!() end
  ##% frozen? : () -> Boolean
  def frozen?() end
  ##% hash : () -> Fixnum
  def hash() end
  ##% include? : t -> Boolean
  def include?(p0) end
  ##% index : t -> Fixnum
  def index(p0) end
  ##% indexes : (*Fixnum) -> Array<t>
  def indexes(*rest) end
  ##% indices : (*Fixnum) -> Array<t>
  def indices(*rest) end
  ##% initialize : (?Fixnum, ?t) -> []
  ##% initialize : Array<t> -> []
  ##% initialize : (Fixnum) {Fixnum -> t} -> []
  def initialize(*rest) end
  ##% insert<u> ; u <= t : (Fixnum, *u) -> Array<t>
  def insert(ind, *rest) end
  ##% inspect : () -> String
  def inspect() end
  ##% join : (?String) -> String
  def join(*rest) end
  ##% last : () -> t
  ##% last : Fixnum -> Array<t>
  def last(*n) end
  ##% length : () -> Fixnum
  def length() end
  ##% map<u> : () {t -> u} -> Array<u>
  def map() end
  ##% map!<u> ; u <= t : () {t -> u} -> Array<u>
  def map!() end
  ##% nitems : () -> Fixnum
  def nitems() end
  ##% pack : String -> String
  def pack(p0) end
  ##% pop : () -> t
  def pop() end
  ##% push<u> ; u <= t : (*u) -> Array<t>
  def push(*rest) end
  ##% rassoc<u> : u -> t # ; t <= Array<u>
  def rassoc(p0) end
  ##% reject<u>: () {t -> u} -> Array<t>
  def reject() end
  ##% reject!<u> : () {t -> u} -> Array<t>
  def reject!() end
  ##% replace<u> ; u <= t : Array<u> -> Array<u>
  def replace(p0) end
  ##% reverse : () -> Array<t>
  def reverse() end
  ##% reverse! : () -> Array<t>
  def reverse!() end
  ##% reverse_each<u> : () {t -> u} -> Array<t>
  def reverse_each() end
  ##% rindex : t -> Fixnum
  def rindex(p0) end
  ##% select<u> : () {t -> u} -> Array<t>
  def select() end
  ##% shift : () -> t
  def shift() end
  ##% size : () -> Fixnum
  def size() end
  ##% slice : Fixnum -> t
  ##% slice : (Fixnum, Fixnum) -> Array<t>
  ##% slice : Range -> Array<t>
  def slice(*rest) end
  ##% slice! : Fixnum -> t
  ##% slice! : (Fixnum, Fixnum) -> Array<t>
  ##% slice! : Range -> Array<t>
  def slice!(p0, *rest) end
  # #% sort<self> ; self <= [each<u>: () {["<=>" : t -> Fixnum] -> u} -> Array<t>] \
  # #% : () -> Array<t>
  # #% sort : () {(t, t) -> Fixnum} -> Array<t>
  # def sort() end
  # #% sort!<self> ; self <= [each<u>: () {["<=>" : t -> Fixnum] -> u} -> Array<t>] \
  # #% : () -> Array<t>
  # #% sort! : () -> Array<t>
  ##% sort! : () {(t, t) -> Fixnum} -> Array<t>
  def sort!() end
  ##% to_a : () -> Array<t>
  def to_a() end
  ##% to_ary : () -> Array<t>
  def to_ary() end
  ##% to_s : () -> String
  def to_s() end
  ##% transpose : () -> Array<t>
  def transpose() end
  ##% uniq : () -> Array<t>
  def uniq() end
  ##% uniq! : () -> Array<t>
  def uniq!() end
  ##% unshift<u> ; u <= t : (*u) -> Array<t>
  def unshift(*rest) end
  ##% values_at : (*(Fixnum or Range)) -> Array<t>
  def values_at(*rest) end
  ##% zip<u> : (*Array<u>) -> Array<Array<u or t> >
  def zip(*rest) end
  ##% "|"<u> ; u <= t : Array<u> -> Array<t>
  def |(p0) end
  ##% Array."[]" : (*t) -> Array<t>
  def Array.[](*rest) end
end

class BaseIO
  include Enumerable

#   class << self
#     ##% open<t>: (?String,?Fixnum) {BaseIO -> t} -> t
#     def open(string="", mode=0) end
#   end

  def initialize(*) end

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
  ##% lineno:() -> Integer
  def lineno() end
  ##% lineno= : Integer -> Integer
  def lineno=(p0) end
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
  ##% reopen<self> : BaseIO -> self
  ##% reopen<self> : (String,String) -> self
  def reopen(*rest) end
  ##% rewind:() -> Fixnum
  def rewind() end
  ##% seek : (Fixnum, Fixnum) -> Fixnum
  def seek(*rest) end
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
  ##% tty?: () -> Boolean
  def tty?() end
  ##% ungetc: Integer -> NilClass
  def ungetc (p0) end
  ##% write: ([to_s: () -> String]) -> Integer
  def write(p0) end
end

##% IO <= BaseIO
class IO
  include Enumerable
  IO::SEEK_SET=0
  IO::SEEK_CUR=1
  IO::SEEK_END=2

  module Constants
    APPEND = 1024
    CREAT = 64
    EXCL = 128
    FNM_CASEFOLD = 8
    FNM_DOTMATCH = 4
    FNM_NOESCAPE = 1
    FNM_PATHNAME = 2
    FNM_SYSCASE = 0
    LOCK_EX = 2
    LOCK_NB = 4
    LOCK_SH = 1
    LOCK_UN = 8
    NOCTTY = 256
    NONBLOCK = 2048
    RDONLY = 0
    RDWR = 2
    SYNC = 4096
    TRUNC = 512
    WRONLY = 1
  end

  include Constants

  ##% "<<"<self> : [to_s : () -> String] -> self
  def <<(p0) end
  ##% binmode<self> : () -> self
  def binmode() end
  ##% close : () -> NilClass
  def close() end
  ##% close_read : () -> NilClass
  def close_read() end
  ##% close_write : () -> NilClass
  def close_write() end
  ##% closed? : () -> Boolean
  def closed?() end
  ##% each<t,self> : (?String) {String -> t} -> self
  def each(sep=$/) end
  ##% each_byte<t,self> : () {Fixnum -> t} -> self
  def each_byte() end
  ##% each_line<t,self> : (?String) {String -> t} -> self
  def each_line(sep=$/) end
  ##% eof : () -> Boolean
  def eof() end
  ##% eof? : () -> Boolean
  def eof?() end
  ##% fcntl : (Numeric, Numeric or String) -> Integer
  def fcntl(integer_cmd, *rest) end
  ##% fileno : () -> Fixnum
  def fileno() end
  ##% flush<self> : () -> self
  def flush() end
  ##% fsync : () -> Fixnum
  def fsync() end
  ##% getc : () -> Fixnum
  def getc() end
  ##% gets : (?String) -> String
  def gets(sep=$/) end
  ##% initialize : (Integer, ?String) -> []
  def initialize(fd, mode="r") end
  ##% inspect : () -> String
  def inspect() end
  ##% ioctl : (Integer, Numeric or String) -> Integer
  def ioctl(integer_cmd, *rest) end
  ##% isatty : () -> Boolean
  def isatty() end
  ##% lineno : () -> Integer
  def lineno() end
  ##% lineno= : Integer -> Integer
  def lineno=(p0) end
  ##% pid : () -> Fixnum
  def pid() end
  ##% pos : () -> Integer
  def pos() end
  ##% pos= : Integer -> Integer
  def pos=(p0) end
  ##% print : (*[to_s : () -> String]) -> NilClass
  def print(*rest) end
  ##% printf : (String, *[to_s : () -> String]) -> NilClass
  def printf(fmt, *rest) end
  ##% putc : (Numeric) -> Fixnum
  ##% putc : (String) -> String
  def putc(p0) end
  ##% puts : (*[to_s : () -> String]) -> NilClass
  def puts(*rest) end
  ##% read : (?Fixnum, ?String) -> String
  def read(length=99, buf="buffer") end
  ##% read_nonblock : (Integer, ?String) -> String
  def read_nonblock(maxlen=99, buf="buffer") end
  ##% readchar : () -> Fixnum
  def readchar() end
  ##% readline : (?String) -> String
  def readline(sep=$/) end
  ##% readlines : (?String) -> Array<String>
  def readlines(sep=$/) end
  ##% readpartial : (Integer, ?String) -> String
  def readpartial(maxlen=99, buf="buffer") end
  ##% reopen<self> : (IO) -> self
  ##% reopen<self> : (String,String) -> self
  def reopen(*rest) end
  ##% rewind : () -> Fixnum
  def rewind() end
  ##% seek : (Fixnum, Fixnum) -> Fixnum
  def seek(amount, whence=SEEK_SET) end
  ##% stat : () -> File::Stat
  def stat() end
  ##% sync : () -> Boolean
  def sync() end
  ##% sync= : Boolean -> Boolean
  def sync=(p0) end
  ##% sysread : Integer -> String
  def sysread(amt=99) end
  ##% sysseek : (Integer, ?Fixnum) -> Integer
  def sysseek(offset, whence=SEEK_SET) end
  ##% syswrite : ([to_s: () -> String]) -> Integer
  def syswrite(p0) end
  ##% tell : () -> Integer
  def tell() end
  ##% to_i : () -> Fixnum
  def to_i() end
  ##% to_io : () -> IO
  def to_io() end
  ##% tty? : () -> Boolean
  def tty?() end
  ##% ungetc : Integer -> NilClass
  def ungetc(p0) end
  ##% write : ([to_s: () -> String]) -> Integer
  def write(p0) end
  ##% write_nonblock : String -> Integer
  def write_nonblock(p0) end
  ##% IO.for_fd : (Integer, ?String) -> IO
  def IO.for_fd(fd, mode="r") end
  ##% IO.foreach<t> : (String, ?String) {String -> t} -> NilClass
  def IO.foreach(name, sep=$/) end
  # #% IO.open : (Integer, ?String) -> IO
  # #% IO.open<t> : (Integer, ?String) {IO -> t} -> t
  ##% IO.open : (*?) -> ?
  def IO.open(fd, mode="r") end
  ##% IO.pipe : () -> Array<IO>
  def IO.pipe() end
  ##% IO.popen : (String, ?String) -> IO
  ##% IO.popen<t> : (String, ?String) {IO -> t} -> t
  def IO.popen(cmd_string, mode="r") end
  ##% IO.read : ([to_str : () -> String], ?Fixnum, ?Fixnum) -> String
  def IO.read(name, length=99, offset=0)  end
  ##% IO.readlines : (String, ?String) -> Array<String>
  def IO.readlines(name, sep=$/) end
  ##% IO.select : (Array<IO>, ?Array<IO>, ?Array<IO>, ?Fixnum) -> Array
  def IO.select(read_array, write_array=[], error_array=[], timeout=0) end
  ##% IO.sysopen : (String, ?String, ?String) -> Fixnum
  def IO.sysopen(path, mode="r", perm="foo") end
end

class Dir
  include Enumerable
  ##% close : () -> NilClass
  def close() end
  ##% each<t> : () {String -> t} -> Dir
  def each() end
  ##% initialize : String -> []
  def initialize(name) end
  ##% path : () -> String
  def path() end
  ##% pos : () -> Integer
  def pos() end
  ##% pos= : Integer -> Integer
  def pos=(p0) end
  ##% read : () -> String
  def read() end
  ##% rewind : () -> Dir
  def rewind() end
  ##% seek : Integer -> Dir
  def seek(p0) end
  ##% tell : () -> Fixnum
  def tell() end
  ##% Dir."[]" : Array<String> -> Array<String>
  ##% Dir."[]" : (*String) -> Array<String>
  def Dir.[](*rest) end
  ##% Dir.chdir : (?String) -> Fixnum
  ##% Dir.chdir<t> : (?String) {String -> t} -> t
  def Dir.chdir(str="$HOME") end
  ##% Dir.chroot : String -> Fixnum
  def Dir.chroot(p0) end
  ##% Dir.delete : String -> Fixnum
  def Dir.delete(p0) end
  ##% Dir.entries : String -> Array<String>
  def Dir.entries(p0) end
  ##% Dir.foreach<t> : (String) {String -> t} -> NilClass
  def Dir.foreach(p0) end
  ##% Dir.getwd : () -> String
  def Dir.getwd() end
  ##% Dir.glob : (Array<String>, ?Fixnum) -> Array<String>
  ##% Dir.glob : (String, ?Fixnum) -> Array<String>
  ##% Dir.glob<t> : (Array<String>, ?Fixnum) {String -> t} -> NilClass
  ##% Dir.glob<t> : (String, ?Fixnum) {String -> t} -> NilClass
  def Dir.glob(*rest) end
  ##% Dir.mkdir : (String, ?Integer) -> Fixnum
  def Dir.mkdir(name, perm=42) end
  ##% Dir.open : String -> Dir
  ##% Dir.open<t> : (String) {Dir -> t} -> t
  def Dir.open(p0) end
  ##% Dir.pwd : () -> String
  def Dir.pwd() end
  ##% Dir.rmdir : String -> Fixnum
  def Dir.rmdir(p0) end
  ##% Dir.unlink : String -> Fixnum
  def Dir.unlink(p0) end
end

module Process
  module GID # FIXME
    ##% GID.change_privilege : !FIXME -> !FIXME
    def GID.change_privilege(*) end
    ##% GID.eid : !FIXME -> !FIXME
    def GID.eid(*) end
    ##% GID.eid= : !FIXME -> !FIXME
    def GID.eid=(*) end
    ##% GID.grant_privilege : !FIXME -> !FIXME
    def GID.grant_privilege(*) end
    ##% GID.re_exchange : !FIXME -> !FIXME
    def GID.re_exchange(*) end
    ##% GID.re_exchangeable? : !FIXME -> !FIXME
    def GID.re_exchangeable?(*) end
    ##% GID.rid : !FIXME -> !FIXME
    def GID.rid(*) end
    ##% GID.sid_available? : !FIXME -> !FIXME
    def GID.sid_available?(*) end
    ##% GID.switch : !FIXME -> !FIXME
    def GID.switch(*) end
  end

  PRIO_PGRP = 1
  PRIO_PROCESS = 0
  PRIO_USER = 2
  RLIMIT_AS = 9
  RLIMIT_CORE = 4
  RLIMIT_CPU = 0
  RLIMIT_DATA = 2
  RLIMIT_FSIZE = 1
  RLIMIT_MEMLOCK = 8
  RLIMIT_NOFILE = 7
  RLIMIT_NPROC = 6
  RLIMIT_RSS = 5
  RLIMIT_STACK = 3
  RLIM_INFINITY = 18446744073709551615
  RLIM_SAVED_CUR = 18446744073709551615
  RLIM_SAVED_MAX = 18446744073709551615

  class Status #FIXME
    ##% "&" : !FIXME -> !FIXME
    def &(*) end
    ##% "==" : !FIXME -> !FIXME
    def ==(*) end
    ##% ">>" : !FIXME -> !FIXME
    def >>(*) end
    ##% coredump? : !FIXME -> !FIXME
    def coredump?(*) end
    ##% exited? : !FIXME -> !FIXME
    def exited?(*) end
    ##% exitstatus : !FIXME -> !FIXME
    def exitstatus(*) end
    ##% inspect : !FIXME -> !FIXME
    def inspect(*) end
    ##% pid : !FIXME -> !FIXME
    def pid(*) end
    ##% signaled? : !FIXME -> !FIXME
    def signaled?(*) end
    ##% stopped? : !FIXME -> !FIXME
    def stopped?(*) end
    ##% stopsig : !FIXME -> !FIXME
    def stopsig(*) end
    ##% success? : !FIXME -> !FIXME
    def success?(*) end
    ##% termsig : !FIXME -> !FIXME
    def termsig(*) end
    ##% to_i : !FIXME -> !FIXME
    def to_i(*) end
    ##% to_int : !FIXME -> !FIXME
    def to_int(*) end
    ##% to_s : !FIXME -> !FIXME
    def to_s(*) end
  end

  module Process::Sys #FIXME
    ##% Sys.getegid : !FIXME -> !FIXME
    def (Process::Sys).getegid(*) end
    ##% Sys.geteuid : !FIXME -> !FIXME
    def (Process::Sys).geteuid(*) end
    ##% Sys.getgid : !FIXME -> !FIXME
    def (Process::Sys).getgid(*) end
    ##% Sys.getuid : !FIXME -> !FIXME
    def (Process::Sys).getuid(*) end
    ##% Sys.issetugid : !FIXME -> !FIXME
    def (Process::Sys).issetugid(*) end
    ##% Sys.setegid : !FIXME -> !FIXME
    def (Process::Sys).setegid(*) end
    ##% Sys.seteuid : !FIXME -> !FIXME
    def (Process::Sys).seteuid(*) end
    ##% Sys.setgid : !FIXME -> !FIXME
    def (Process::Sys).setgid(*) end
    ##% Sys.setregid : !FIXME -> !FIXME
    def (Process::Sys).setregid(*) end
    ##% Sys.setresgid : !FIXME -> !FIXME
    def (Process::Sys).setresgid(*) end
    ##% Sys.setresuid : !FIXME -> !FIXME
    def (Process::Sys).setresuid(*) end
    ##% Sys.setreuid : !FIXME -> !FIXME
    def (Process::Sys).setreuid(*) end
    ##% Sys.setrgid : !FIXME -> !FIXME
    def (Process::Sys).setrgid(*) end
    ##% Sys.setruid : !FIXME -> !FIXME
    def (Process::Sys).setruid(*) end
    ##% Sys.setuid : !FIXME -> !FIXME
    def (Process::Sys).setuid(*) end
  end

  module Process::UID #FIXME
    ##% UID.change_privilege : !FIXME -> !FIXME
    def (Process::UID).change_privilege(*) end
    ##% UID.eid : !FIXME -> !FIXME
    def (Process::UID).eid(*) end
    ##% UID.eid= : !FIXME -> !FIXME
    def (Process::UID).eid=(*) end
    ##% UID.grant_privilege : !FIXME -> !FIXME
    def (Process::UID).grant_privilege(*) end
    ##% UID.re_exchange : !FIXME -> !FIXME
    def (Process::UID).re_exchange(*) end
    ##% UID.re_exchangeable? : !FIXME -> !FIXME
    def (Process::UID).re_exchangeable?(*) end
    ##% UID.rid : !FIXME -> !FIXME
    def (Process::UID).rid(*) end
    ##% UID.sid_available? : !FIXME -> !FIXME
    def (Process::UID).sid_available?(*) end
    ##% UID.switch : !FIXME -> !FIXME
    def (Process::UID).switch(*) end
  end

  WNOHANG = 1
  WUNTRACED = 2

  ## % FIXME
  ##% Process.abort : !FIXME -> !FIXME
  def Process.abort(*) end

  ##% Process.detach : Fixnum -> Thread
  def Process.detach(p0) end
  ##% Process.egid : () -> Fixnum
  def Process.egid() end
  ##% Process.egid= : Fixnum -> Fixnum
  def Process.egid=(p0) end
  ##% Process.euid : () -> Fixnum
  def Process.euid() end
  ##% Process.euid= : Integer -> Integer
  def Process.euid=(p0) end

  # FIXME
  ##% Process.exit : !FIXME -> !FIXME
  def Process.exit(*) end
  ##% Process.exit! : !FIXME -> !FIXME
  def Process.exit!(*) end
  ##% Process.fork : !FIXME -> !FIXME
  def Process.fork(*) end


  ##% Process.getpgid : Integer -> Integer
  def Process.getpgid(p0) end

  # FIXME
  ##% Process.getpgrp : !FIXME -> !FIXME
  def Process.getpgrp(*) end

  ##% Process.getpriority : (Fixnum, Integer) -> Fixnum
  def Process.getpriority(p0, p1) end
  ##% Process.getrlimit : Fixnum -> Array<Fixnum>
  def Process.getrlimit(p0) end
  ##% Process.gid : () -> Fixnum
  def Process.gid() end
  ##% Process.gid= : Fixnum -> Fixnum
  def Process.gid=(p0) end
  ##% Process.groups : () -> Array<Fixnum>
  def Process.groups() end
  ##% Process.groups= : Array<Fixnum> -> Array<Fixnum>
  def Process.groups=(p0) end
  ##% Process.initgroups : (String, Fixnum) -> Array<Fixnum>
  def Process.initgroups(p0, p1) end
  ##% Process.kill : (Integer or String, Fixnum, *Fixnum) -> Fixnum
  def Process.kill(sig, *rest) end
  ##% Process.maxgroups : () -> Fixnum
  def Process.maxgroups() end
  ##% Process.maxgroups= : Fixnum -> Fixnum
  def Process.maxgroups=(p0) end
  ##% Process.pid : () -> Fixnum
  def Process.pid() end
  ##% Process.ppid : () -> Fixnum
  def Process.ppid() end
  ##% Process.setpgid : (Fixnum, Integer) -> Fixnum
  def Process.setpgid(p0, p1) end
  ##% Process.setpgrp : () -> Fixnum
  def Process.setpgrp() end
  ##% Process.setpriority : (Fixnum, Integer, Fixnum) -> Fixnum
  def Process.setpriority(p0, p1, p2) end
  ##% Process.setrlimit : (Fixnum, Fixnum, ?Fixnum) -> NilClass
  def Process.setrlimit(*rest) end
  ##% Process.setsid : () -> Fixnum
  def Process.setsid() end
  ##% Process.times : () -> Array<Float>
  def Process.times() end
  ##% Process.uid : () -> Fixnum
  def Process.uid() end
  ##% Process.uid= : Integer -> Integer
  def Process.uid=(p0) end
  ##% Process.wait : (?Fixnum, ?Fixnum) -> Fixnum
  def Process.wait(pid=-1, flags=0) end
  ##% Process.wait2 : (?Fixnum, ?Fixnum) -> Array<Fixnum>
  def Process.wait2(pid=-1, flags=0) end
  ##% Process.waitall : () -> Array<Array<Fixnum> >
  def Process.waitall() end
  ##% Process.waitpid : (?Fixnum, ?Fixnum) -> Fixnum
  def Process.waitpid(pid=-1, flags=0) end
  ##% Process.waitpid2 : (?Fixnum, ?Fixnum) -> Array<Fixnum>
  def Process.waitpid2(pid=-1, flags=0) end
end

class File < IO
  include Enumerable

  ALT_SEPARATOR = nil

  module Constants
    include IO::Constants
  end
  include Constants

  PATH_SEPARATOR = ":"
  SEPARATOR = "/"
  Separator = "/"

  class File::Stat # FIXME
    include Comparable
    ##% "<=>"<t> : t -> Fixnum
    def <=>(p0) end
    ##% atime : () -> Time
    def atime() end
    ##% blksize : () -> Integer
    def blksize() end
    ##% blockdev? : () -> Boolean
    def blockdev?() end
    ##% blocks : () -> Integer
    def blocks() end
    ##% chardev? : () -> Boolean
    def chardev?() end
    ##% ctime : () -> Time
    def ctime() end
    ##% dev : () -> Fixnum
    def dev() end
    ##% dev_major : () -> Fixnum
    def dev_major() end
    ##% dev_minor : () -> Fixnum
    def dev_minor() end
    ##% directory? : () -> Boolean
    def directory?() end
    ##% executable? : () -> Boolean
    def executable?() end
    ##% executable_real? : () -> Boolean
    def executable_real?() end
    ##% file? : () -> Boolean
    def file?() end
    ##% ftype : () -> String
    def ftype() end
    ##% gid : () -> Fixnum
    def gid() end
    ##% grpowned? : () -> Boolean
    def grpowned?() end
    ##% initialize : (String) -> []
    def initialize(*) end
    ##% ino : () -> Fixnum
    def ino() end
    ##% inspect : () -> String
    def inspect() end
    ##% mode : () -> Fixnum
    def mode() end
    ##% mtime : () -> Time
    def mtime() end
    ##% nlink : () -> Fixnum
    def nlink() end
    ##% owned? : () -> Boolean
    def owned?() end
    ##% pipe? : () -> Boolean
    def pipe?() end
    ##% rdev : () -> Fixnum
    def rdev() end
    ##% rdev_major : () -> Fixnum
    def rdev_major() end
    ##% rdev_minor : () -> Fixnum
    def rdev_minor() end
    ##% readable? : () -> Boolean
    def readable?() end
    ##% readable_real? : () -> Boolean
    def readable_real?() end
    ##% setgid? : () -> Boolean
    def setgid?() end
    ##% setuid? : () -> Boolean
    def setuid?() end
    ##% size : () -> Integer
    def size() end
    ##% size? : () -> Boolean
    def size?() end
    ##% socket? : () -> Boolean
    def socket?() end
    ##% sticky? : () -> Boolean
    def sticky?() end
    ##% symlink? : () -> Boolean
    def symlink?() end
    ##% uid : () -> Fixnum
    def uid() end
    ##% writable? : () -> Boolean
    def writable?() end
    ##% writable_real? : () -> Boolean
    def writable_real?() end
    ##% zero? : () -> Boolean
    def zero?() end
  end

  ##% atime : () -> Time
  def atime() end
  ##% chmod : Integer -> Fixnum
  def chmod(p0) end
  ##% chown : (Integer, Integer) -> Fixnum
  def chown(p0, p1) end
  ##% ctime : () -> Time
  def ctime() end
  ##% flock : Integer -> Fixnum or Boolean
  def flock(p0) end
  ##% initialize : (String, ?(String or Fixnum), ?Fixnum) -> []
  def initialize(filename, mode="r", perm=0) end
  ##% lstat : () -> File::Stat # DAVIDAN
  def lstat() end
  ##% mtime : () -> Time
  def mtime() end
  ##% path : () -> String
  def path() end
  ##% truncate : Integer -> Fixnum
  def truncate(p0) end
  ##% File.atime : String -> Time
  def File.atime(p0) end
  ##% File.basename : ([to_str : () -> String], ?String) -> String
  def File.basename(file_name, suffix=".foo") end
  ##% File.blockdev? : String -> Boolean
  def File.blockdev?(p0) end
  ##% File.chardev? : String -> Boolean
  def File.chardev?(p0) end
  ##% File.chmod : (Integer, *String) -> Fixnum
  def File.chmod(mode, *files) end
  ##% File.chown : (Fixnum, Fixnum, *String) -> Integer
  def File.chown(owner_int, group_int, *files) end
  ##% File.ctime : String -> Time
  def File.ctime(p0) end
  ##% File.delete : (*String) -> Integer
  def File.delete(*files) end
  ##% File.directory? : String -> Boolean
  def File.directory?(p0) end
  ##% File.dirname : [to_str : () -> String] -> String
  def File.dirname(p0) end
  ##% File.executable? : String -> Boolean
  def File.executable?(p0) end
  ##% File.executable_real? : String -> Boolean
  def File.executable_real?(p0) end
  ##% File.exist? : String -> Boolean
  def File.exist?(p0) end
  ##% File.exists? : String -> Boolean
  def File.exists?(p0) end
  ##% File.expand_path : (String, ?String) -> String
  def File.expand_path(file_name, dir_string=".")  end
  ##% File.extname : String -> String
  def File.extname(p0) end
  ##% File.file? : String -> Boolean
  def File.file?(p0) end
  ##% File.fnmatch : (String, String, ?Fixnum) -> Boolean
  def File.fnmatch(pattern, path, flags=0) end
  ##% File.fnmatch? : (String, String, ?Fixnum) -> Boolean
  def File.fnmatch?(pattern, path, flags=0) end
#  def File.for_fd(*rest) end # TODO - from IO
#  def File.foreach(*rest) end # TODO - from IO
  ##% File.ftype : String -> String
  def File.ftype(p0) end
  ##% File.grpowned? : String -> Boolean
  def File.grpowned?(p0) end
  ##% File.identical? : (String, String) -> Boolean
  def File.identical?(p0, p1) end
  ##% File.join : (*[to_str:() -> String]) -> String
  ##% File.join : (Array<[to_str:() -> String]>) -> String
  def File.join(*rest) end
  ##% File.lchmod : (Integer, *String) -> Integer
  def File.lchmod(mode, *files) end
  ##% File.lchown : (Integer, Integer, *String) -> Integer
  def File.lchown(owner_int, group_int, *files) end
  ##% File.link : (String, String) -> Fixnum
  def File.link(p0, p1) end
  ##% File.lstat : [to_str:() -> String] -> File::Stat # DAVIDAN
  def File.lstat(p0) end
  ##% File.mtime : String -> Time
  def File.mtime(p0) end
  ##% File.owned? : String -> Boolean
  def File.owned?(p0) end
  ##% File.pipe? : String -> Boolean
  def File.pipe?(p0) end
  ##% File.readable? : String -> Boolean
  def File.readable?(p0) end
  ##% File.readable_real? : String -> Boolean
  def File.readable_real?(p0) end
  ##% File.readlink : String -> String
  def File.readlink(p0) end
  ##% File.rename : (String, String) -> Fixnum
  def File.rename(p0, p1) end
  ##% File.setgid? : String -> Boolean
  def File.setgid?(p0) end
  ##% File.setuid? : String -> Boolean
  def File.setuid?(p0) end
  ##% File.size : String -> Fixnum
  def File.size(p0) end
  ##% File.size? : String -> Integer
  def File.size?(p0) end
  ##% File.socket? : String -> Boolean
  def File.socket?(p0) end
  ##% File.split : String -> Array<String>
  def File.split(p0) end
  ##% File.stat : String -> File::Stat
  def File.stat(p0) end
  ##% File.sticky? : String -> Boolean
  def File.sticky?(p0) end
  ##% File.symlink : (String, String) -> Fixnum
  def File.symlink(p0, p1) end
  ##% File.symlink? : String -> Boolean
  def File.symlink?(p0) end
  ##% File.truncate : (String, Integer) -> Fixnum
  def File.truncate(p0, p1) end
  ##% File.umask : (?Integer) -> Integer
  def File.umask(m=0) end
  ##% File.unlink : (*String) -> Integer
  def File.unlink(*files) end
  ##% File.utime : (Time, Time, *String) -> Integer
  def File.utime(at, mt, *files) end
  ##% File.writable? : String -> Boolean
  def File.writable?(p0) end
  ##% File.writable_real? : String -> Boolean
  def File.writable_real?(p0) end
  ##% File.zero? : String -> Boolean
  def File.zero?(p0) end
end

module FileTest # FIXME

  ##% FileTest.blockdev? : String -> Boolean
  def FileTest.blockdev?(p0) end
  ##% FileTest.chardev? : String -> Boolean
  def FileTest.chardev?(p0) end
  ##% FileTest.directory? : String -> Boolean
  def FileTest.directory?(p0) end
  ##% FileTest.executable? : String -> Boolean
  def FileTest.executable?(p0) end
  ##% FileTest.executable_real? : String -> Boolean
  def FileTest.executable_real?(p0) end
  ##% FileTest.exist? : String -> Boolean
  def FileTest.exist?(p0) end

  ##% FileTest.exists? : String -> Boolean
  def FileTest.exists?(p0) end

  ##% FileTest.file? : String -> Boolean
  def FileTest.file?(p0) end
  ##% FileTest.grpowned? : String -> Boolean
  def FileTest.grpowned?(p0) end
  ##% FileTest.identical? : !FIXME -> !FIXME
  def FileTest.identical?(*) end
  ##% FileTest.owned? : String -> Boolean
  def FileTest.owned?(p0) end
  ##% FileTest.pipe? : String -> Boolean
  def FileTest.pipe?(p0) end
  ##% FileTest.readable? : String -> Boolean
  def FileTest.readable?(p0) end
  ##% FileTest.readable_real? : String -> Boolean
  def FileTest.readable_real?(p0) end
  ##% FileTest.setgid? : String -> Boolean
  def FileTest.setgid?(p0) end
  ##% FileTest.setuid? : String -> Boolean
  def FileTest.setuid?(p0) end
  ##% FileTest.size : String -> Fixnum
  def FileTest.size(p0) end
  ##% FileTest.size? : String -> Fixnum
  def FileTest.size?(p0) end
  ##% FileTest.socket? : String -> Boolean
  def FileTest.socket?(p0) end
  ##% FileTest.sticky? : String -> Boolean
  def FileTest.sticky?(p0) end
  ##% FileTest.symlink? : String -> Boolean
  def FileTest.symlink?(p0) end
  ##% FileTest.writable? : String -> Boolean
  def FileTest.writable?(p0) end
  ##% FileTest.writable_real? : String -> Boolean
  def FileTest.writable_real?(p0) end
  ##% FileTest.zero? : String -> Boolean
  def FileTest.zero?(p0) end
end

##% Proc<^args,ret>
class Proc
  # #% "==" : Proc<^other,oret> -> Boolean
  ##% "=="<t> : t -> Boolean
  def ==(*) end
  ##% "[]" : (^args) -> ret
  def [](*rest) end # alias for call
  ##% arity : () -> Fixnum
  def arity() end
  ##% binding : () -> Binding
  def binding() end
  ##% call : (^args) -> ret
  def call() end
  ##% inspect : () -> String
  def inspect() end
  ##% to_proc : () -> Proc<^args, ret>
  def to_proc() end
  ##% to_s : () -> String
  def to_s() end
  ##% initialize : () {^args -> ret} -> []
  def initialize(); end
end

class Time
  include Comparable
  ##% "+" : Numeric -> Time
  def +(p0) end
  ##% "-" : Time -> Float
  ##% "-" : Numeric -> Time
  def -(p0) end
  ##% "<=>"<t> : t -> Fixnum
  def <=>(p0) end
  ##% _dump : () -> String
  def _dump() end # was *rest
  ##% asctime : () -> String
  def asctime() end
  ##% ctime : () -> String
  def ctime() end
  ##% day : () -> Fixnum
  def day() end
  ##% dst? : () -> Boolean
  def dst?() end
  ##% eql?<t> : t -> Boolean #Time -> Boolean
  def eql?(p0) end
  ##% getgm : () -> Time
  def getgm() end
  ##% getlocal : () -> Time
  def getlocal() end
  ##% getutc : () -> Time
  def getutc() end
  ##% gmt? : () -> Boolean
  def gmt?() end
  ##% gmt_offset : () -> Fixnum
  def gmt_offset() end
  ##% gmtime : () -> Time
  def gmtime() end
  ##% gmtoff : () -> Fixnum
  def gmtoff() end
  ##% hash : () -> Fixnum
  def hash() end
  ##% hour : () -> Fixnum
  def hour() end
  ##% inspect : () -> String
  def inspect() end
  ##% isdst : () -> Boolean
  def isdst() end
  ##% localtime : () -> Time
  def localtime() end
  ##% mday : () -> Fixnum
  def mday() end
  ##% min : () -> Fixnum
  def min() end
  ##% mon : () -> Fixnum
  def mon() end
  ##% month : () -> Fixnum
  def month() end
  ##% sec : () -> Fixnum
  def sec() end
  ##% strftime : String -> String
  def strftime(p0) end
  ##% succ : () -> Time
  def succ() end
  ##% to_a : () -> Array<Fixnum or String>
  def to_a() end
  ##% to_f : () -> Float
  def to_f() end
  ##% to_i : () -> Fixnum
  def to_i() end
  ##% to_s : () -> String
  def to_s() end
  ##% tv_sec : () -> Integer
  def tv_sec() end
  ##% tv_usec : () -> Integer
  def tv_usec() end
  ##% usec : () -> Integer
  def usec() end
  ##% utc : () -> Time
  def utc() end
  ##% utc? : () -> Boolean
  def utc?() end
  ##% utc_offset : () -> Fixnum
  def utc_offset() end
  ##% wday : () -> Fixnum
  def wday() end
  ##% yday : () -> Fixnum
  def yday() end
  ##% year : () -> Fixnum
  def year() end
  ##% zone : () -> String
  def zone() end
  ##% Time._load : String -> Time
  def Time._load(p0) end
  ##% Time.at : Time -> Time
  ##% Time.at : (Numeric, ?Numeric) -> Time
  def Time.at(secs, micro=0) end
  ##% Time.gm : (Numeric, ?Numeric, ?Numeric, ?Numeric, ?Numeric, ?Numeric, \
  ##%            ?Numeric) -> Time
  ##% Time.gm : (Numeric, Numeric, Numeric, Numeric, Numeric, Numeric, \
  ##%            Numeric, Numeric, Boolean, String) -> Time
  def Time.gm(year_or_sec, month_or_min=1, day_or_hour=1, hour_or_day=0, min_or_month=0, sec_or_year=0, usec_or_wday=0, yday=0, isdst=true, zone="UTC") end
  ##% Time.local : (Numeric, ?Numeric, ?Numeric, ?Numeric, ?Numeric, ?Numeric,\
  ##%               ?Numeric) -> Time
  ##% Time.local : (Numeric, Numeric, Numeric, Numeric, Numeric, Numeric, \
  ##%               Numeric, Numeric, Boolean, String) -> Time
  def Time.local(year_or_sec, month_or_min=1, day_or_hour=1, hour_or_day=0, min_or_month=0, sec_or_year=0, usec_or_wday=0, yday=0, isdst=true, zone="GMT")  end
  ### XXX: there is actually a single form of mktime()
  ##% Time.mktime : (Numeric, Numeric, Numeric, Numeric, Numeric, Numeric, \
  ##%                Numeric) -> Time
  def Time.mktime(year_or_sec, month_or_min=1, day_or_hour=1, hour_or_day=0, min_or_month=0, sec_or_year=0, usec_or_wday=0, yday=0, isdst=true, zone="GMT") end
  ##% Time.now : () -> Time
  def Time.now() end # was *rest
#  def Time.times() end  # TODO deprecated
  ##% Time.utc : (Numeric, ?Numeric, ?Numeric, ?Numeric, ?Numeric, ?Numeric, \
  ##%             ?Numeric) -> Time
  ##% Time.utc : (Numeric, Numeric, Numeric, Numeric, Numeric, Numeric, \
  ##%             Numeric, Numeric, Boolean, String) -> Time
  def Time.utc(year_or_sec, month_or_min=1, day_or_hour=1, hour_or_day=0, min_or_month=0, sec_or_year=0, usec_or_wday=0, yday=0, isdst=true, zone="GMT") end
end

class Data
  # MikeF: This class really does appear to be empty
  # (at least initially!)
end

# MikeF: These are all empty too
##% StandardError <= Exception
class StandardError < Exception;end
##% ScriptError <= Exception
class ScriptError < Exception;end
##% LoadError <= ScriptError
class LoadError < ScriptError;end
##% NotImplementedError <= ScriptError
class NotImplementedError < ScriptError;end
##% SyntaxError <= ScriptError
class SyntaxError < ScriptError;end
##% NoMemoryError <= Exception
class NoMemoryError < Exception;end
##% SignalException <= Exception
class SignalException < Exception;end
##% SystemStackError <= StandardError
class SystemStackError < StandardError;end
##% RuntimeError <= StandardError
class RuntimeError < StandardError;end
##% TypeError <= StandardError
class TypeError < StandardError;end
##% ArgumentError <= StandardError
class ArgumentError < StandardError;end
##% IOError <= StandardError
class IOError < StandardError;end
##% ThreadError <= StandardError
class ThreadError < StandardError;end
##% ZeroDivisionError <= StandardError
class ZeroDivisionError < StandardError;end
##% EOFError <= IOError
class EOFError < IOError;end
##% RangeError <= StandardError
class RangeError < StandardError;end
##% FloatDomainError <= RangeError
class FloatDomainError < RangeError;end
##% RegexpError <= StandardError
class RegexpError < StandardError;end
##% Interrupt <= SignalException
class Interrupt < SignalException;end
##% IndexError <= StandardError
class IndexError < StandardError;end
##% SecurityError <= StandardError
class SecurityError < StandardError;end

##% NameError <= StandardError
class NameError < StandardError #FIXME
  ##% name : !FIXME -> !FIXME
  def name(*) end
  ##% to_s : () -> String
  def to_s() end
end

##% NoMethodError <= NameError
class NoMethodError < NameError #FIXME
  ##% initialize : (String, [to_s : () -> String], *[]) -> NoMethodError
  def initialize(*args); end
  ##% args : !FIXME -> !FIXME
  def args(*) end
end

##% SystemCallError <= StandardError
class SystemCallError < StandardError #FIXME
  ##% errno : !FIXME -> !FIXME
  def errno(*) end
  ##% SystemCallError."===" : !FIXME -> !FIXME
  def SystemCallError.===(*) end
end

##% LocalJumpError <= StandardError
class LocalJumpError < StandardError #FIXME
  ##% exit_value : !FIXME -> !FIXME
  def exit_value(*) end
  ##% reason : !FIXME -> !FIXME
  def reason(*) end
end

##% SystemExit <= Exception
class SystemExit < Exception #FIXME
  ##% status : () -> Fixnum
  def status() end
  ##% success? : () -> Boolean
  def success?() end
end

module Errno
  class Errno::E2BIG < SystemCallError
    Errno = 7
  end
  class Errno::EACCES < SystemCallError
    Errno = 13
  end
  class Errno::EADDRINUSE < SystemCallError
    Errno = 98
  end
  class Errno::EADDRNOTAVAIL < SystemCallError
    Errno = 99
  end
  class Errno::EADV < SystemCallError
    Errno = 68
  end
  class Errno::EAFNOSUPPORT < SystemCallError
    Errno = 97
  end
  class Errno::EAGAIN < SystemCallError
    Errno = 11
  end
  class Errno::EALREADY < SystemCallError
    Errno = 114
  end
  class Errno::EBADE < SystemCallError
    Errno = 52
  end
  class Errno::EBADF < SystemCallError
    Errno = 9
  end
  class Errno::EBADFD < SystemCallError
    Errno = 77
  end
  class Errno::EBADMSG < SystemCallError
    Errno = 74
  end
  class Errno::EBADR < SystemCallError
    Errno = 53
  end
  class Errno::EBADRQC < SystemCallError
    Errno = 56
  end
  class Errno::EBADSLT < SystemCallError
    Errno = 57
  end
  class Errno::EBFONT < SystemCallError
    Errno = 59
  end
  class Errno::EBUSY < SystemCallError
    Errno = 16
  end
  class Errno::ECHILD < SystemCallError
    Errno = 10
  end
  class Errno::ECHRNG < SystemCallError
    Errno = 44
  end
  class Errno::ECOMM < SystemCallError
    Errno = 70
  end
  class Errno::ECONNABORTED < SystemCallError
    Errno = 103
  end
  class Errno::ECONNREFUSED < SystemCallError
    Errno = 111
  end
  class Errno::ECONNRESET < SystemCallError
    Errno = 104
  end
  class Errno::EDEADLK < SystemCallError
    Errno = 35
  end
  class Errno::EDEADLK < SystemCallError
    Errno = 35
  end
  class Errno::EDESTADDRREQ < SystemCallError
    Errno = 89
  end
  class Errno::EDOM < SystemCallError
    Errno = 33
  end
  class Errno::EDOTDOT < SystemCallError
    Errno = 73
  end
  class Errno::EDQUOT < SystemCallError
    Errno = 122
  end
  class Errno::EEXIST < SystemCallError
    Errno = 17
  end
  class Errno::EFAULT < SystemCallError
    Errno = 14
  end
  class Errno::EFBIG < SystemCallError
    Errno = 27
  end
  class Errno::EHOSTDOWN < SystemCallError
    Errno = 112
  end
  class Errno::EHOSTUNREACH < SystemCallError
    Errno = 113
  end
  class Errno::EIDRM < SystemCallError
    Errno = 43
  end
  class Errno::EILSEQ < SystemCallError
    Errno = 84
  end
  class Errno::EINPROGRESS < SystemCallError
    Errno = 115
  end
  class Errno::EINTR < SystemCallError
    Errno = 4
  end
  class Errno::EINVAL < SystemCallError
    Errno = 22
  end
  class Errno::EIO < SystemCallError
    Errno = 5
  end
  class Errno::EISCONN < SystemCallError
    Errno = 106
  end
  class Errno::EISDIR < SystemCallError
    Errno = 21
  end
  class Errno::EISNAM < SystemCallError
    Errno = 120
  end
  class Errno::EL2HLT < SystemCallError
    Errno = 51
  end
  class Errno::EL2NSYNC < SystemCallError
    Errno = 45
  end
  class Errno::EL3HLT < SystemCallError
    Errno = 46
  end
  class Errno::EL3RST < SystemCallError
    Errno = 47
  end
  class Errno::ELIBACC < SystemCallError
    Errno = 79
  end
  class Errno::ELIBBAD < SystemCallError
    Errno = 80
  end
  class Errno::ELIBEXEC < SystemCallError
    Errno = 83
  end
  class Errno::ELIBMAX < SystemCallError
    Errno = 82
  end
  class Errno::ELIBSCN < SystemCallError
    Errno = 81
  end
  class Errno::ELNRNG < SystemCallError
    Errno = 48
  end
  class Errno::ELOOP < SystemCallError
    Errno = 40
  end
  class Errno::EMFILE < SystemCallError
    Errno = 24
  end
  class Errno::EMLINK < SystemCallError
    Errno = 31
  end
  class Errno::EMSGSIZE < SystemCallError
    Errno = 90
  end
  class Errno::EMULTIHOP < SystemCallError
    Errno = 72
  end
  class Errno::ENAMETOOLONG < SystemCallError
    Errno = 36
  end
  class Errno::ENAVAIL < SystemCallError
    Errno = 119
  end
  class Errno::ENETDOWN < SystemCallError
    Errno = 100
  end
  class Errno::ENETRESET < SystemCallError
    Errno = 102
  end
  class Errno::ENETUNREACH < SystemCallError
    Errno = 101
  end
  class Errno::ENFILE < SystemCallError
    Errno = 23
  end
  class Errno::ENOANO < SystemCallError
    Errno = 55
  end
  class Errno::ENOBUFS < SystemCallError
    Errno = 105
  end
  class Errno::ENOCSI < SystemCallError
    Errno = 50
  end
  class Errno::ENODATA < SystemCallError
    Errno = 61
  end
  class Errno::ENODEV < SystemCallError
    Errno = 19
  end
  class Errno::ENOENT < SystemCallError
    Errno = 2
  end
  class Errno::ENOEXEC < SystemCallError
    Errno = 8
  end
  class Errno::ENOLCK < SystemCallError
    Errno = 37
  end
  class Errno::ENOLINK < SystemCallError
    Errno = 67
  end
  class Errno::ENOMEM < SystemCallError
    Errno = 12
  end
  class Errno::ENOMSG < SystemCallError
    Errno = 42
  end
  class Errno::ENONET < SystemCallError
    Errno = 64
  end
  class Errno::ENOPKG < SystemCallError
    Errno = 65
  end
  class Errno::ENOPROTOOPT < SystemCallError
    Errno = 92
  end
  class Errno::ENOSPC < SystemCallError
    Errno = 28
  end
  class Errno::ENOSR < SystemCallError
    Errno = 63
  end
  class Errno::ENOSTR < SystemCallError
    Errno = 60
  end
  class Errno::ENOSYS < SystemCallError
    Errno = 38
  end
  class Errno::ENOTBLK < SystemCallError
    Errno = 15
  end
  class Errno::ENOTCONN < SystemCallError
    Errno = 107
  end
  class Errno::ENOTDIR < SystemCallError
    Errno = 20
  end
  class Errno::ENOTEMPTY < SystemCallError
    Errno = 39
  end
  class Errno::ENOTNAM < SystemCallError
    Errno = 118
  end
  class Errno::ENOTSOCK < SystemCallError
    Errno = 88
  end
  class Errno::ENOTTY < SystemCallError
    Errno = 25
  end
  class Errno::ENOTUNIQ < SystemCallError
    Errno = 76
  end
  class Errno::ENXIO < SystemCallError
    Errno = 6
  end
  class Errno::EOPNOTSUPP < SystemCallError
    Errno = 95
  end
  class Errno::EOVERFLOW < SystemCallError
    Errno = 75
  end
  class Errno::EPERM < SystemCallError
    Errno = 1
  end
  class Errno::EPFNOSUPPORT < SystemCallError
    Errno = 96
  end
  class Errno::EPIPE < SystemCallError
    Errno = 32
  end
  class Errno::EPROTO < SystemCallError
    Errno = 71
  end
  class Errno::EPROTONOSUPPORT < SystemCallError
    Errno = 93
  end
  class Errno::EPROTOTYPE < SystemCallError
    Errno = 91
  end
  class Errno::ERANGE < SystemCallError
    Errno = 34
  end
  class Errno::EREMCHG < SystemCallError
    Errno = 78
  end
 class Errno::EREMOTE < SystemCallError
    Errno = 66
  end
  class Errno::EREMOTEIO < SystemCallError
    Errno = 121
  end
  class Errno::ERESTART < SystemCallError
    Errno = 85
  end
  class Errno::EROFS < SystemCallError
    Errno = 30
  end
  class Errno::ESHUTDOWN < SystemCallError
    Errno = 108
  end
  class Errno::ESOCKTNOSUPPORT < SystemCallError
    Errno = 94
  end
  class Errno::ESPIPE < SystemCallError
    Errno = 29
  end
  class Errno::ESRCH < SystemCallError
    Errno = 3
  end
  class Errno::ESRMNT < SystemCallError
    Errno = 69
  end
  class Errno::ESTALE < SystemCallError
    Errno = 116
  end
  class Errno::ESTRPIPE < SystemCallError
    Errno = 86
  end
  class Errno::ETIME < SystemCallError
    Errno = 62
  end
  class Errno::ETIMEDOUT < SystemCallError
    Errno = 110
  end
  class Errno::ETOOMANYREFS < SystemCallError
    Errno = 109
  end
  class Errno::ETXTBSY < SystemCallError
    Errno = 26
  end
  class Errno::EUCLEAN < SystemCallError
    Errno = 117
  end
  class Errno::EUNATCH < SystemCallError
    Errno = 49
  end
  class Errno::EUSERS < SystemCallError
    Errno = 87
  end
  class Errno::EAGAIN < SystemCallError
    Errno = 11
  end
  class Errno::EXDEV < SystemCallError
    Errno = 18
  end
  class Errno::EXFULL < SystemCallError
    Errno = 54
  end

  EDEADLOCK = Errno::EDEADLK
  EWOULDBLOCK = Errno::EAGAIN
end

class Binding #FIXME
  ##% clone : !FIXME -> !FIXME
  def clone(*) end
  ##% dup : !FIXME -> !FIXME
  def dup(*) end
end

class Continuation #FIXME
  ##% "[]" : !FIXME -> !FIXME
  def [](*) end
  ##% call : !FIXME -> !FIXME
  def call(*) end
end

module GC #FIXME
  ##% garbage_collect : !FIXME -> !FIXME
  def garbage_collect(*) end
  ##% GC.disable : !FIXME -> !FIXME
  def GC.disable(*) end
  ##% GC.enable : !FIXME -> !FIXME
  def GC.enable(*) end
  ##% GC.start : !FIXME -> !FIXME
  def GC.start(*) end
end

module Marshal #FIXME
  MAJOR_VERSION = 4
  MINOR_VERSION = 8
  ##% Marshal.dump<t> : (t,?Fixnum) -> String
  ##% Marshal.dump<t,io> ; io <= BaseIO : (t,io,?Fixnum) -> io
  def Marshal.dump(*rest) end
  ### the proc is passed *every* (sub-)object that is deserialized
  ##% Marshal.load<t> : (BaseIO or [to_str: () -> String],?Proc<^(Object),NilClass>) -> t
  def Marshal.load(*rest) end
  ##% Marshal.restore : !FIXME -> !FIXME
  def Marshal.restore(*rest) end
end

module Math #FIXME
  E = 2.71828182845905
  PI = 3.14159265358979
  ##% Math.acos : !FIXME -> !FIXME
  def Math.acos(p0) end
  ##% Math.acosh : !FIXME -> !FIXME
  def Math.acosh(p0) end
  ##% Math.asin : !FIXME -> !FIXME
  def Math.asin(p0) end
  ##% Math.asinh : !FIXME -> !FIXME
  def Math.asinh(p0) end
  ##% Math.atan : !FIXME -> !FIXME
  def Math.atan(p0) end
  ##% Math.atan2 : !FIXME -> !FIXME
  def Math.atan2(*) end
  ##% Math.atanh : !FIXME -> !FIXME
  def Math.atanh(*) end
  ##% Math.cos : !FIXME -> !FIXME
  def Math.cos(*) end
  ##% Math.cosh : !FIXME -> !FIXME
  def Math.cosh(*) end
  ##% Math.erf : !FIXME -> !FIXME
  def Math.erf(*) end
  ##% Math.erfc : !FIXME -> !FIXME
  def Math.erfc(*) end
  ##% Math.exp : Numeric -> Float
  def Math.exp(p0) end
  ##% Math.frexp : !FIXME -> !FIXME
  def Math.frexp(*) end
  ##% Math.hypot : !FIXME -> !FIXME
  def Math.hypot(*) end
  ##% Math.ldexp : !FIXME -> !FIXME
  def Math.ldexp(*) end
  ##% Math.log : Numeric -> Float
  def Math.log(p0) end
  ##% Math.log10 : !FIXME -> !FIXME
  def Math.log10(p0) end
  ##% Math.sin : !FIXME -> !FIXME
  def Math.sin(p0) end
  ##% Math.sinh : !FIXME -> !FIXME
  def Math.sinh(p0) end
  ##% Math.sqrt : Numeric -> Float
  def Math.sqrt(p0) end
  ##% Math.tan : !FIXME -> !FIXME
  def Math.tan(p0) end
  ##% Math.tanh : !FIXME -> !FIXME
  def Math.tanh(p0) end
end

##% Method<^args,ret>
class Method
  ##% "=="<t> : t -> Boolean
  def ==(p0) end
  ##% "[]" : (^args) -> ret
  def [](*rest) end
  ##% arity : () -> Fixnum
  def arity() end
  ##% call : (^args) -> ret
  def call(*) end
  ##% clone<self> : () -> self
  def clone(*) end
  ##% inspect : () -> String
  def inspect() end
  ##% to_proc : () -> Proc<^args,ret>
  def to_proc() end
  ##% to_s : () -> String
  def to_s() end
  ##% unbind : () -> UnboundMethod<^args,ret>
  def unbind() end
end

module Signal #FIXME
  ##% Signal.list : !FIXME -> !FIXME
  def Signal.list(*) end
  ##% Signal.trap : !FIXME -> !FIXME
  def Signal.trap(*) end
end

class Struct #FIXME
  class Tms < Struct #FIXME
    include Enumerable
    ##% cstime : !FIXME -> !FIXME
    def cstime(*) end
    ##% cstime= : !FIXME -> !FIXME
    def cstime=(*) end
    ##% cutime : !FIXME -> !FIXME
    def cutime(*) end
    ##% cutime= : !FIXME -> !FIXME
    def cutime=(*) end
    ##% initialize : !FIXME -> !FIXME
    def initialize(*) end
    ##% stime : !FIXME -> !FIXME
    def stime(*) end
    ##% stime= : !FIXME -> !FIXME
    def stime=(*) end
    ##% utime : !FIXME -> !FIXME
    def utime(*) end
    ##% utime= : !FIXME -> !FIXME
    def utime=(*) end
    ##% Tms."[]" : !FIXME -> !FIXME
    def Tms.[](*) end
    ##% Tms.members : !FIXME -> !FIXME
    def Tms.members(*) end
  end

  include Enumerable
  ##% "==" : !FIXME -> !FIXME
  def ==(*) end
  ##% "[]" : !FIXME -> !FIXME
  def [](*) end
  ##% "[]=" : !FIXME -> !FIXME
  def []=(*) end
  ##% each : !FIXME -> !FIXME
  def each(*) end
  ##% each_pair : !FIXME -> !FIXME
  def each_pair(*) end
  ##% eql? : !FIXME -> !FIXME
  def eql?(*) end
  ##% hash : !FIXME -> !FIXME
  def hash(*) end
  ##% initialize : (String, *Symbol) -> []
  def initialize(*rest) end
  ##% inspect : !FIXME -> !FIXME
  def inspect(*) end
  ##% length : !FIXME -> !FIXME
  def length(*) end
  ##% members : !FIXME -> !FIXME
  def members(*) end
  ##% select : !FIXME -> !FIXME
  def select(*rest) end
  ##% size : !FIXME -> !FIXME
  def size(*) end
  ##% to_a : !FIXME -> !FIXME
  def to_a(*) end
  ##% to_s : !FIXME -> !FIXME
  def to_s(*) end
  ##% values : !FIXME -> !FIXME
  def values(*) end
  ##% values_at : !FIXME -> !FIXME
  def values_at(*rest) end
end

##% UnboundMethod<^args,ret>
class UnboundMethod #FIXME
  ##% "==" : !FIXME -> !FIXME
  def ==(*) end
  ##% arity : !FIXME -> !FIXME
  def arity(*) end
  ##% bind : (?) -> Method<^args,ret>
  def bind(p0) end
  ##% clone<self> : () -> self
  def clone(*) end
  ##% inspect : !FIXME -> !FIXME
  def inspect(*) end
  ##% to_s : !FIXME -> !FIXME
  def to_s(*) end
end

##############################
# Non-class Object constants #
##############################

ARGF = Object.new
class << ARGF  #FIXME
=begin
From the rubinius definition of $< (same as ARGF):

An object that provides access to the concatenation of the contents of
all the files given as command-line arguments or $stdin (in the case
where there are no arguments). $< supports methods similar to a File
object: binmode, close, closed?, each, each_byte, each_line, eof,
eof?, file, filename, fileno, getc, gets, lineno, lineno=, path, pos,
pos=, read, readchar, readline, readlines, rewind, seek, skip, tell,
to_a, to_i, to_io, to_s, along with the methods in Enumerable. The
method file returns a File object for the file currently being
read. This may change as $< reads through the files on the command
line. [r/o]

In irb, ARGF is not actually an instance of a class, but rather
 irb> ARGF.class
 => Object

=end
end

ARGV = Array.new(1,"args")
ENV = Hash.new # {"HOME" => "Foo"}
FALSE = false
NIL = nil
PLATFORM="i486-linux"
RELEASE_DATE="2008-03-03"
RUBY_PATCHLEVEL=114
RUBY_PLATFORM="i486-linux"
RUBY_RELEASE_DATE="2008-03-03"
RUBY_VERSION="2.3.3"
STDERR=IO.new 3
STDIN=IO.new 1
STDOUT=IO.new 2
TOPLEVEL_BINDING=Binding.new
TRUE=true
VERSION="2.3.3"

# See http://ruby-doc.org/core/files/lib/English_rb.html for list of many of these
#$! = nil # last error message - can't reliable produce; not sure of type
$@ = ["location"] # execution stack of error
$; = "" # default split pattern
$, = "" # default output separator string
$/ = "\n" # input record separator
$\ = "" # output record separator
$. = 100 # last line number read
$_ = "gets" # local scope; last string read
$< = ARGF
$> = $stdout
$$ = 42 # pid; readonly
# $? = Process::Status.new # exit status of last child process
$~ = MatchData.new # local scope
$= = false # case-insensitivity flag
$* = ["args"] # command line argument
$` = "prematch"
$' = "postmatch"
$+ = "highestmatch"
$& = "last-match" # string last matched
$0 = "progname" # name of ruby script file
$: = ["libraries"]
$" = ["loaded-files"]
$1 = "matched"
$2 = "matched"
$3 = "matched"
$4 = "matched"
$5 = "matched"
$6 = "matched"
$7 = "matched"
$8 = "matched"
$9 = "matched" # local scope; $~[i]
# Note: Can't write to $`, $', $+, $&, $i


module YAML
  # #% YAML.dump<t> : (t, ?BaseIO) -> String
  ##% YAML.dump<t,u> : (t, ?u) -> String
  def YAML.dump(p0, *rest) end
  ##% YAML.load_file : String -> ?
  def YAML.load_file(*) end
  ##% YAML.load : String -> ?
  ##% YAML.load : [read : (?Fixnum, ?String) -> String] -> ?
  def YAML.load(*) end
  ##% YAML.load_documents<t> : (BaseIO) {? -> t} -> t
  def YAML.load_documents(*) end
  ##% YAML.quick_emit<t> : (Fixnum,?Hash) {YAML::Emitter -> t} -> String
  def YAML.quick_emit(obj_id, opts) end

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
