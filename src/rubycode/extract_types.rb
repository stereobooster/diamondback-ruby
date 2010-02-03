# usage: ruby -r <requires> $0 <classes>
# set envvar EXTRACT_ALL to print out all classes (not just cmdline ones)

@orig_constants = Object.constants

$classes = {}
$seen = {}
$extracted = {}
$outputted = {}

class Afoo
  class Bfoo
    class W
      def m() end
    end
    Z = W
    class B < Z
    end
    
  end
end

def n_fresh(n) 
  if n < 0 then
    arr = []
    required = (-n) - 1
    required.times {|i| arr.push "p#{i}"}
    arr.push "*rest"
    arr.join(", ")
  else
    arr = []
    n.times {|i| arr.push "p#{i}"}
    arr.join(", ")
  end
end

def class_mod(x) x.instance_of?(Class) || x.instance_of?(Module) end
def class_mod_obj(x) class_mod(x) || x.instance_of?(Object) end

class Extraction
  attr_reader :ruby_class

  def eql?(other)
    to_s.eql?(other.to_s)
  end
  def hash()
    to_s.hash
  end
  def indent(amt,*args)
    amt.times {print " "}
    print(*args)
  end

end

class Klass < Extraction

  attr_accessor :name, :mixins, :inherits
  attr_accessor :inst_methods, :singleton_methods, :constants


  def to_s()
    "Klass(#{name.inspect})"
  end

  def inspect() to_s() end

  def initialize(name)
    @ruby_class = name
    @name = name
    @mixins = {}
    @inherits = nil
    @inst_methods = {}
    @singleton_methods = {}
    @constants = {}
  end

  def sub_constant(k)
    #print "sub_constant: #{ruby_class.inspect} #{k.ruby_class.inspect}: "
    my_ap = ruby_class.inspect.split("::")
    k_ap = k.ruby_class.inspect.split("::")
    res = false
    res = while(!my_ap.nil? && !k_ap.nil?)
            x = my_ap.shift
            y = k_ap.shift
            if x.nil? && y.nil?
              break true
            end
            if y.nil? then 
              #printf "y nil? %s %s\n",ruby_class.inspect,k.ruby_class.inspect
              break false 
            end
            if x.nil? then break true end
            if x == y then next end
            break false
          end
    #printf "%s\n", res.to_s
    return res
  end

  def as_dep() [self] end

  def all_deps()
    return [] if name == Object
    acc = []
    if not @inherits.nil?
      acc = acc | [@inherits]
    end
    @mixins.each_pair do |k,*|
      #printf "dep1 %s mixes %s\n", self.to_s,k
      fail "not klass? #{k}" if not k.instance_of? Klass
      next if ruby_class == IO && k.ruby_class == File::Constants
      if k.name != Object 
        acc = acc | k.as_dep | k.external_deps
      end
    end
    @constants.each_pair do |k,v|
      #printf "dep2 %s constant %s %s\n", self.to_s, k, v.class
      acc = acc | v.as_dep | v.external_deps
      #printf "done\n"
    end
    return acc
  end

  def external_deps()
    all_deps.select {|x| not sub_constant(x)}
  end

  def print_k(show,margin=0)
    short_name=ruby_class.inspect.split("::")[-1]
    if show
      if ruby_class.instance_of? Class
        indent margin, "class ",short_name
        if @inherits then
          print " < ", @inherits.ruby_class.inspect
        end
      elsif ruby_class.instance_of? Module
        indent margin, "module ",ruby_class.inspect
      else
        raise "WTF: #{ruby_class.inspect}"
      end
      print "\n"

      margin += 2
      aliases = {}
      sub_classes={}
      if ruby_class != Object then
        topsort(@constants,show,margin)
      end

      @mixins.each_key do |k|
        next if k.ruby_class == Kernel
        indent margin, "include #{k.ruby_class.inspect}\n"
      end

      @inst_methods.sort.each do |(m,a)|
        if m =~ /^[^a-zA-Z]/ then qname = '"' << m << '"'
        else qname = m end
        indent margin,"##% #{qname} : !FIXME -> !FIXME\n"
        indent margin,"def #{m}(",n_fresh(a),") end\n"
      end

      @singleton_methods.sort.each do |(m,a)|
        if m =~ /^[^a-zA-Z]/ then qname = '"' << m << '"'
        else qname = m end
        indent margin,"##% #{short_name}.#{qname} : !FIXME -> !FIXME\n"
        indent margin,"def #{short_name}.#{m}(",n_fresh(a),") end\n"
      end
      margin -= 2
      indent margin,"end\n\n"
    else
      if ruby_class != Object then
        topsort(@constants,show,margin)
      end
    end
  end
end

class Const < Extraction
  attr_accessor :name, :value
  attr_reader :ruby_class
  def initialize(name,value)
    @name = name
    @value = value
    @ruby_class = value
  end
  def to_s()
    "Const(#{name},#{@ruby_class.inspect})" 
  end
  def as_dep() [] end
  def inspect() to_s() end

  def external_deps() [] end

  def print_k(show,margin=0)
    if show
      indent margin, "#{name} = #{value.inspect}\n"
    end
  end

end

class Alias < Extraction
  attr_accessor :name, :value
  attr_reader :ruby_class
  def initialize(name,value)
    @name = name
    @value = value
    @ruby_class = value
  end
  def as_dep()
    [self] 
  end
  def external_deps()
    [init(Klass.new(ruby_class))]
  end
  def to_s()
    "Alias(#{name},#{@ruby_class.inspect})" 
  end
  def print_k(show,margin=0)
    if show
      indent margin, "#{name} = #{value.inspect}\n"
    end
  end
end

def init(kls)
  name = kls.name
  $seen[kls] = 1
  if $classes.has_key? name
    return $classes[name]
  else
    $classes[name] = kls
    return kls
  end
end

def record_method(kls,name,meth)
  #puts "saw #{meth.inspect}"

  case meth.inspect
  when /Method: ([^#]+)\((.*)\)#(.+)>$/
    ancestor = $2
    #printf "ancestor1 %s\n", ancestor
    mod = eval(ancestor)
    kmod = init(Klass.new(mod))
    if mod.class == Module && mod != Kernel then
      kls.mixins[kmod] = 1
    end

  when /^#<UnboundMethod: ([^#]+)#(.+)>$/
    kls.inst_methods[name] = meth.arity

  when /^#<Method: ([^.]+)\((.*)\)\.(.+)>$/

    ancestor = $2
    #printf "ancestor2 %s\n", ancestor
    mod = eval(ancestor)
    kmod=init(Klass.new(mod))
    # $classes[mod][:singleton_methods][name] = meth.arity
  when /^#<Method: ([^.]+)\.(.+)>$/
    kls.singleton_methods[name] = meth.arity

  when /^#<Method: ([^#\(]+)#(.+)>$/
    ancestor = $1
    #printf "ancestor3 %s\n", ancestor
    raise "WTF" if $1 == name
    mod = eval(ancestor)
    init(Klass.new(mod))
  else 
    fail "other #{meth.inspect}"
  end
end

def extract_class_methods(extract)
  clazz = extract.ruby_class
  #printf "extracting from %s\n", clazz.inspect
  kls = init(Klass.new(clazz))
  if clazz.kind_of? Class then
    if clazz.superclass != Object then
      kls.inherits = init(Klass.new(clazz.superclass))
    end
  end

  clazz.respond_to? :included_modules and clazz.included_modules.each {|x|
    inc_kls = init(Klass.new(x))
    kls.mixins[inc_kls] = 1
  }

  clazz.public_methods.each {|m|
    next if m.to_s == "`"
    meth = clazz.method(m);
    record_method(kls,m,meth)
  }

  if class_mod clazz then
    clazz.public_instance_methods.each {|m|
      next if m.to_s == "`"
      meth = clazz.instance_method(m);
      record_method(kls,m,meth)
    }
    clazz.constants.each do |const|
      if clazz.const_defined? const then
        const_name = if clazz == Object then const else clazz.inspect + "::" + const end
        const_e = clazz.const_get const
        if (class_mod const_e) && (const_name == const_e.inspect)
          const_cls = Klass.new(const_e)
        elsif class_mod const_e
          #printf "alias? %s = %s\n", const_name, const_e.inspect
          const_cls = Alias.new(const,const_e)
        else
          const_cls = Const.new(const,const_e)
        end
        const_cls = init(const_cls)
        #printf "storing %s = %s\n",const,const_cls.inspect
        kls.constants[const] = const_cls

        #printf "storing constant %s in %s\n",const,clazz.inspect
      end
    end
  end
end

def extract_methods(o)
  if not o.kind_of? Extraction
    puts "expecting extraction object, got #{o.inspect}(#{o.class}) instead"
    exit(1)
  end

  #if not class_mod(o) then return end


  if $extracted.key?(o) then return end

  #puts "extracting #{o.inspect}"

  #print "extracting methods from #{o.inspect}\n"
  $extracted[o] = 1

  extract_class_methods(o)
end

def class_fixpoint()
  while not $seen.empty? do
    look_at = {}
    $seen.each_key do |k|
      if not $extracted.key?(k)
        look_at[k] = 1
      end
    end
    $seen = {}
    look_at.each_key do |k|
      extract_methods(k)
    end
  end
end


def output_class(clazz,ch,margin=0)
  if clazz.instance_of? Module then
    indent margin,"module #{clazz.inspect}"
  elsif clazz.instance_of? Class then
    indent margin,"class #{clazz.inspect}"
  else 
    fail "wtf: #{clazz.to_s}"
  end
  
  if ch[:inherits] then
    print " < ", ch[:inherits].to_s
  end
  print "\n"

  aliases = {}
  sub_classes={}
  if clazz != Object then
    ch[:constants].each_pair do |k,v|
      const_name = clazz.inspect + "::" + k
      if class_mod_obj(v) && const_name == v.inspect then
        sub_classes[v] = $classes[v]
        #output_class(v,$classes[v],margin+2)
        #      elsif class_mod(v)
        #        aliases[k] = v
      else
        aliases[k] = v
        #        indent margin, "  #{k} = #{v.inspect}\n"
      end
    end
  end

  printf "topsorting for %s\n",clazz.inspect
  topsort_and_print(sub_classes)
  aliases.sort.each do |(k,v)|
    indent margin, "  #{k} = #{v.inspect}\n"
  end

  ch[:mixins].each_key do |k|
    next if k == Kernel
    indent margin, "  include #{k}\n"
  end

  ch[:inst_methods].sort.each do |(m,a)|
    indent margin,"  def #{m}(",n_fresh(a),") end\n"
    #indent margin,n_fresh(a)
    #puts ") end"
  end

  ch[:singleton_methods].sort.each do |(m,a)|
    short_name=clazz.inspect.split("::")[-1]
    indent margin,"  def #{short_name}.#{m}(",n_fresh(a),") end\n"
    #indent margin,n_fresh(a)
    #puts ") end"
  end

  indent margin,"end\n\n"
  @outputted[clazz]=1
end

def mark_outputted(clazz)
  @outputted[clazz]=1
  return if clazz.instance_of? String
  ch = $classes[clazz]
  ch[:constants].sort.each do |(k,v)|
    if class_mod_obj(v) && clazz != Object then
      mark_outputted(v)
    end
  end
end

def topsort_and_print(worklist)
  # topological sort
  while worklist.size > 0 do
    revisit_classes = {}
    worklist.each_pair do |clazz,ch|
      if clazz.instance_of? String then
        #if clazz == ch.inspect then raise "WTF #{clazz}" end
        if not @outputted.key?(ch.inspect)
          revisit_classes[clazz] = ch
        end
      else 
        if (ch[:inherits] && !@outputted.key?(ch[:inherits])) then
          printf "%s inherits %s\n", clazz.inspect, ch[:inherits]
          revisit_classes[clazz] = ch
          next
        end
        
        if clazz != Kernel && ch[:mixins].select { |k,*|
            break(0) if clazz == Kernel
            next(false) if clazz == IO && k.to_s == "File::Constants" 
            
            next(false) if /::/.match(clazz.inspect)
            # fixme to use Module#nesting instead
            next(false) if k.inspect.include?(clazz.inspect)
            if (!@outputted.key?(k)) then
              printf "%s includes %s\n", clazz.inspect, k.inspect
              true
            else 
              false
            end
          }.size > 0  
        then
          revisit_classes[clazz] = ch
          next
        end
      end

      
      #next if /::/.match(clazz.inspect)

      if ENV.include?("EXTRACT_BASE") || ARGV.include?(clazz.inspect) then
        if class_mod_obj(clazz)
          output_class(clazz,ch)
        end
      else
        mark_outputted clazz
      end
      #printf "instance is %s\n", clazz.class.inspect
    end
    worklist = revisit_classes
  end
end

def print_it(k,v)
  printf "print: %s %s\n", k.inspect, v.inspect
end

def topsort(worklist,show,margin=0)
  while not worklist.empty?
    fixpoint = {}
    worklist.each_pair do |k,v|
      deps = v.external_deps
      if not (deps.all? {|d| $outputted.key?(d)}) then
        fixpoint[k] = v
      else
        $outputted[v] = 1
        if show ||
            (ENV.include?("EXTRACT_ALL") ||
             ARGV.include?(v.ruby_class.inspect)) 
        then
          v.print_k(true,margin)
        else 
          v.print_k(false,margin)
        end
      end
    end
    worklist = fixpoint
  end

end

def main()
  if ARGV.size < 1 then 
    print "usage: ruby -r <requires> #{$0} <classes>\n"
    exit(1)
  end
  obj_kls = init(Klass.new(Object))
  extract_methods(obj_kls)
  ARGV.each do |arg|
    ap = arg.split("::")
    acc = Object
    ap[0...-1].each do |x|
      acc = acc.const_get 
    end
    clazz = acc.const_get(ap[-1])
    extract_methods(Klass.new(clazz))
  end
  class_fixpoint
  topsort($classes[Object].constants,false)
end


main
