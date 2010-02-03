
# == Description
#
# Type profiling module:
#   Automatically instruments all methods for modules loaded/created
#   after this one, observes (nominal) parameter and return types for
#   method calls, and prints a list of locations/type annotations on
#   program exit.  Intended for use with RubySpec or some other
#   extensive testing system (has not been tested with anything but
#   rubyspec/mspec).

#========================================

module TypeAnnotation

  class ExcludeClasses

    # classes to *not* profile (specifically those found in
    # rspec or top-level classes that we don't want to trace)
    @@excluded_classes = {
      "Module" => true,
      "Class"  => true,
      "Object" => true,
      "Kernel" => true,
      "MSpec"  => true,
      "MSpecRun" => true,
      "MSpecScript" => true,
      "MSpecOptions" => true,
      "SpecGuard" => true,
      "SpecPositiveOperatorMatcher" => true,
      "SpecVersion" => true,
      "Tally" => true,
      "TallyAction" => true,
      "TimerAction" => true,
      "VariableMatcher" => true,
      "VersionGuard" => true,
      "ContextState" => true,
      "DottedFormatter" => true,
      "ExampleState" => true,
    }

    # ignore annotations for modules on these paths
    @@excluded_paths = {
      "ruby/mspec" => true,
      "ruby/rubyspec" => true,
      "ruby/rubyspec" => true,
      "ruby/rubyspec" => true,
    }

    def self.add(clazz)
      @@excluded_classes[clazz] = true
    end

    def self.excluded?(clazz)
      @@excluded_classes.has_key?(clazz.to_s)
    end

    def self.is_path_excluded?(p)
      @@excluded_paths.each_key {|k|
        return true if(p.index(k) != nil)
      }
      return false
    end

  end

  class TypeAnnotation
    # contains a single method signature
    attr_accessor :args, :ret, :block_args, :block_ret

    def initialize(args, ret, has_block=false, block_args=nil, block_ret = nil)
      @args = args
      @ret = ret
      @has_block = has_block
      @block_args = block_args
      @block_ret = block_ret
    end

    def to_s
      # do some cleanup to better match DRuby's type annotation
      # language
      args = @args == nil ? "" : ((@args.map {|a| a.to_s}).join(', '))
      ret  = @ret  == nil ? "NilClass" : @ret.to_s
      args.gsub!(/(True|False)Class/, 'Boolean')
      ret.gsub!(/(True|False)Class/, 'Boolean')
      block_suffix = ""
      if(@has_block)
        bargs = @block_args == nil ? "" :
          ((@block_args.map {|a| a.to_s}).join(', '))
        bret  = @block_ret  == nil ? "NilClass" : @block_ret.to_s
        bargs.gsub!(/(True|False)Class/, 'Boolean')
        bret.gsub!(/(True|False)Class/, 'Boolean')
        block_suffix = " {(#{bargs}) -> #{bret}}"
      end
      "(#{args})#{block_suffix} -> #{ret}"
    end

  end

  class TypeAnnotationCollection
    attr_accessor :meth, :file, :line

    def initialize(meth, file, line)
      @file = file
      @line = line
      @meth = meth
      @types = Hash.new()
    end

    def add_annotation(args, ret, has_block=false,
                       block_args=nil, block_rets=nil)
      if(!has_block)
        t = TypeAnnotation.new(args, ret)
        s_of_t = t.to_s
        if(!@types.has_key?(s_of_t))
          @types[s_of_t] = t
        end
        t

      else
        # add block trace wrapper if this method takes a block
        if(block_args.length == 0)
          $stderr.puts "WARNING: method has block, " +
            "but block args has length 0 for #{@file}:#{@line}:#{@meth}" if
            ! ExcludeClasses.is_path_excluded?(@file)
          t = TypeAnnotation.new(args, ret, has_block, ["!FIXME"], ["!FIXME"])
          s_of_t = t.to_s
          if(!@types.has_key?(s_of_t))
            @types[s_of_t] = t
          end

        else
          # capture all argument types
          for i in 0...block_args.length
            t = TypeAnnotation.new(args, ret, has_block,
                                   block_args[i], block_rets[i])
            s_of_t = t.to_s
            if(!@types.has_key?(s_of_t))
              @types[s_of_t] = t
            end
          end

        end
      end
      nil
    end

    def annotation_types()
      @types
    end

  end

end

#========================================

class Module
  @@inserting_type_data = false
  @@inserting_module_function_type_data = false
  @@identified_types = {}

  # synchronize instrumentation saving in case the profiling is
  # in done in a multi-threaded environment
  # (don't know if this is necessary, but we only want to instrument
  # methods once so it seems a good idea)
  def self.lock_instrumentation_mutex
    if(@@inserting_type_data == false)
      @@inserting_type_data = true
      return true
    end
    return false
  end

  def self.unlock_instrumentation_mutex
    if(@@inserting_type_data == false)
       false
     end
    @@inserting_type_data = false
    true
  end


  def self.lock_module_function_instrumentation_mutex
    if(@@inserting_module_function_type_data == false)
      @@inserting_module_function_type_data = true
      return true
    end
    return false
  end

  def self.unlock_module_function_instrumentation_mutex
    if(@@inserting_module_function_type_data == false)
       false
     end
    @@inserting_module_function_type_data = false
    true
  end

  def identified_types
    @@identified_types
  end

  @@count = 1
  def count
    c = @@count
    @@count += 1
    c
  end

  def self.dump_type_annotations
    # print out all the type annotations we've collected --
    # should be called on program exit
    @@identified_types.sort {|a, b| a <=> b}.each {|k, v|
      if(v != nil)
        types = v.annotation_types
        types.each_key {|t|
          # skip excluded classes/paths
          next if TypeAnnotation::ExcludeClasses.is_path_excluded?(v.file)
          pos = "[#{v.file}@#{v.line}] "
          puts pos + "##% " + k.to_s + " : " + t.to_s
        }
      end
    }
    nil
  end

  def self.derive_type(v)
    # heuristic to guess nominal values for type variables in collections
    if(v.class.to_s == "Array")
      classes = {}
      v.each {|i| classes[i.class] = true}
      if(classes.keys.length > 0)
        tparams = classes.keys.map{|x| x.to_s}.sort.join(' or ')
        return "Array<#{tparams}>"
      end

    elsif(v.class.to_s == "Hash")
      key_classes = {}
      val_classes = {}
      v.each_pair {|key, val|
        key_classes[key.class] = true
        val_classes[val.class] = true
      }

      # TODO:  this over-generalizes the type of this object
      # in that it ignores any dependencies between key and value types
      # (by taking the Cartesian product of all types that occur
      # in the hash), but I don't think that it's that big of a problem
      kparams = ""
      vparams = ""
      if(key_classes.keys.length > 0)
        kparams = key_classes.keys.sort{|a, b| a.to_s <=> b.to_s}.join(' or ')
        vparams = val_classes.keys.sort{|a, b| a.to_s <=> b.to_s}.join(' or ')
        return "Hash<#{kparams}, #{vparams}>"
      end

    elsif(v.class == "Range")
      first_type = v.first.class # assume the types of values are the same
      return "Range<#{first_type}>"
    end # TODO:  other collections?

    return v.class.to_s
  end

  #========================================
  # install profiling wrappers for instance methods

  def insert_type_capture(m, override=false)
    # don't instrument excluded classes
    return if(TypeAnnotation::ExcludeClasses.excluded?(self))

    return if(self.class == Module)

    return if !override && !Module.lock_instrumentation_mutex

    # create a slightly different method name so we can install an
    # alias for the newly created method
    orig = ""
    if /^[a-zA-Z_]*$/ =~ m.to_s
      orig = "noncapture_#{count()}_#{m}".to_sym
    else
      orig = "noncapture_#{count()}".to_sym
    end

    sig = self.to_s + ":" + m.to_s;

    self.class_eval {
      begin
        define_method(orig,instance_method(m))
      rescue NameError => e
        if(m_obj == nil)
          if(!Module.unlock_instrumentation_mutex)
            raise "bad instrumentation mutex unlock for #{sig}"
          end
          return
        end
      end

      # store the location of this annotation for later reference
      file = ""
      line = 0
      if /^([^:]+):(\d+)/ =~ caller[3]
        file = $1
        line = $2.to_i
      else
        # fall back to this file when we can't find the caller --
        # this isn't great, but maybe it's better than nothing
        file = __FILE__
        line = __LINE__+3
      end

      # create a new annotation container for this method
      this_collection =
        TypeAnnotation::TypeAnnotationCollection.new(m, file, line)
      @@identified_types[sig] = this_collection

      # generate a new type-capturing method wrapper, then eval it into place
      code = <<-EOF
        def #{m}(*args,&blk)
          my_class = eval "#{self}"
          sig = "#{sig}"
          my_types = Module.identified_types[sig]
          r = nil
          new_blk = blk
          has_blk = false
          blk_arg_types = []
          blk_ret_types = []

          if block_given?
            has_blk = true
            old_blk = blk
            new_blk = Proc.new {|*block_args|
              if(block_args)
                blk_arg_types.push(block_args.map{|a| Module.derive_type(a)})
              else
                blk_arg_types.push(nil)
              end
              block_r = old_blk.call(*block_args)
              blk_ret_types.push(Module.derive_type(block_r))
              block_r
            }
          end

          if args then
            r = #{orig}(*args,&new_blk)
          else
            r = #{orig}(&new_blk)
          end

          if(my_types != nil)
            my_types.add_annotation(args.map {|a| Module.derive_type(a)},
                                    Module.derive_type(r), has_blk,
                                    blk_arg_types, blk_ret_types)
          end

          r

        end
        EOF

        eval code, binding(), file, line
      }

    if(!override)
      if(!Module.unlock_instrumentation_mutex)
        raise "bad instrumentation mutex unlock for #{sig}"
      end
    end

  end

  # copied/adapted from DRuby's signature.rb
  # replace method added to install profiling hook with each new
  # method definition
  alias :old_added :method_added
  def method_added(meth)
    if(meth.to_s =~ /method_added/ || meth.to_s =~ /old_singleton_added/ ||
       meth.to_s =~ /insert(_singleton)?_type_capture/)
      return
    end

    # call old method_added first
    old_added(meth)
    # then wrap with capturing method variant
    insert_type_capture(meth) unless
      meth == :initialize || meth == :method_added
  end

  #========================================
  # install profiling wrappers for module functions

  def insert_module_function_type_capture(m, override=false)
    return if(TypeAnnotation::ExcludeClasses.excluded?(self))

    return if !override && !Module.lock_module_function_instrumentation_mutex

    return if(m =~ /^noncapture_module_function_/)

    orig = ""
    if /^[a-zA-Z_]+[a-zA-Z_0-9]*$/ =~ m.to_s
      orig = "noncapture_module_function_#{count()}_#{m}".to_sym
    else
      orig = "noncapture_module_function_#{count()}".to_sym
    end
    sig = "(module) #{self}.#{m}"

    begin
      begin
        if(self.class.to_s == "Module")
          m_obj = self.method(m)
          if(m_obj == nil)
            if(!Module.unlock_module_function_instrumentation_mutex)
              raise "bad instrumentation mutex unlock for #{sig}"
            end
            return
          end
          begin
            m_old = self.method(orig)
            $stderr.puts "ERROR:  method #{orig} already exists"
          rescue
            #puts "#{orig} does not exist, all is well (r)"
          end
        else
          define_method(orig,instance_method(m))
        end
      rescue NameError => e
        if(m_obj == nil)
          if(!Module.unlock_module_function_instrumentation_mutex)
            raise "bad instrumentation mutex unlock for #{sig}"
          end
          return
        end
      end

      file = ""
      line = 0
      if /^([^:]+):(\d+)/ =~ caller[3]
        file = $1
        line = $2.to_i
      else
        file = __FILE__
        line = __LINE__+3
      end

      this_collection = TypeAnnotation::TypeAnnotationCollection.new(m, file, line)
      @@identified_types[sig] = this_collection

      # NOTE: using eval to create a module function doesn't seem to
      # work correctly -- instead, we use procs with alias_method(),
      # define_method(), and module_function() to get the wrapper
      # installed in the right place

      # alias this method, then make *that* a module function
      self.send(:alias_method, orig, m)
      self.send(:module_function, orig.to_sym)

      m_proc = Proc.new {|*args, &blk|
        my_types = Module.identified_types[sig]
        r = nil
        new_blk = blk
        has_blk = false
        blk_arg_types = []
        blk_ret_types = []

        # NOTE: DO NOT use has_block? -- always returns false for
        # tests w/ module functions?
        if (blk != nil && blk.class.to_s == "Proc")
          has_blk = true
          old_blk = blk
          new_blk = Proc.new {|*block_args|
            if(block_args)
              blk_arg_types.push(block_args.map{|a| Module.derive_type(a)})
            else
              blk_arg_types.push(nil)
            end
            block_r = old_blk.call(*block_args)
            blk_ret_types.push(Module.derive_type(block_r))
            block_r
          }
        end
        if args then
          r = self.send(orig.to_sym, *args, &new_blk)
        else
          r = self.send(orig.to_sym, &new_blk)
        end
        if(my_types != nil)
          my_types.add_annotation(args.map {|a| Module.derive_type(a)},
                                  Module.derive_type(r), has_blk,
                                  blk_arg_types, blk_ret_types)
        end
        r
      }

      # now install the aliased version, making sure that the renamed
      # module function is still available (duplicate calls don't hurt)
      self.send(:define_method, m.to_sym, m_proc)
      self.send(:module_function, m.to_sym)
      self.send(:module_function, orig.to_sym)

    end

    if(!override)
      if(!Module.unlock_module_function_instrumentation_mutex)
        raise "bad instrumentation mutex unlock for #{sig}"
      end
    end

  end

  alias :old_mod_function :module_function
  def module_function(*args)
    # add a similar wrapper for module functions, which have their own callback
    # module_function gets called with some strange syntax

    if(args.length > 0) # shouldn't this always happen?
      # TODO:  I don't know that I've seen this work on real ruby modules...
      for s in args
        old_mod_function(s, *args)
        insert_module_function_type_capture(s)
      end
    end
  end

end

class Object

  #========================================
  # install profiling wrappers for singleton (class) methods

  def insert_singleton_type_capture(m)

    return if(TypeAnnotation::ExcludeClasses.excluded?(self))

    if !Module.lock_instrumentation_mutex then
      return
    else
      orig = ""
      if /^[a-zA-Z_]*$/ =~ m.to_s
        orig = "noncapture_singleton_#{Module.count()}_#{m}".to_sym
      else
        orig = "noncapture_singleton_#{Module.count()}".to_sym
      end

      #neat trick from <http://stackoverflow.com/questions/803020/\
      #redefining-a-single-ruby-method-on-a-single-instance-with-a-lambda>
      singleton_class = class << self; self; end
      singleton_class.send(:alias_method, orig, m)
      # end neat trick

      sig = "#{self.name}:self.#{m}"
      singleton_class.class_eval do
        file = ""
        line = 0
        if /^([^:]+):(\d+)/ =~ caller[3]
          file = $1
          line = $2.to_i
        else
          file = __FILE__
          line = __LINE__+3
        end

        this_collection = TypeAnnotation::TypeAnnotationCollection.new(m, file, line)
        Module.identified_types[sig] = this_collection

        eval <<-EOF, binding(), file, line
        def #{m}(*args,&blk)
          sig = "#{sig}"
          s = ""
          my_types = Module.identified_types[sig]
          r = nil
          new_blk = blk
          has_blk = false
          blk_arg_types = []
          blk_ret_types = []

          if block_given?
            has_blk = true
            old_blk = blk
            new_blk = Proc.new {|*block_args|
              if(block_args)
                blk_arg_types.push(block_args.map{|a| Module.derive_type(a)})
              else
                blk_arg_types.push(nil)
              end
              block_r = old_blk.call(*block_args)
              blk_ret_types.push(Module.derive_type(block_r))
              block_r
            }
          end

          if args then
            r = #{orig}(*args,&new_blk)
          else
            r = #{orig}(&new_blk)
          end

          if(my_types != nil)
            my_types.add_annotation(args.map {|a| Module.derive_type(a)},
                                    Module.derive_type(r), has_blk,
                                    blk_arg_types, blk_ret_types)
          end
          r
        end
        EOF
      end

      if(!Module.unlock_instrumentation_mutex)
        raise "bad instrumentation mutex unlock for #{sig}"
      end
    end
  end

  # do the same thing for singleton methods in object that we did for
  # instance methods in module
  alias :old_singleton_added :singleton_method_added
  def self.singleton_method_added(m)
    meth = m.id2name

    # don't instrument instrumentors
    return if(meth.to_s =~ /method_added/ || meth.to_s =~ /old_singleton_added/)

    old_singleton_added(meth)
    insert_singleton_type_capture(meth) unless
      meth == :initialize || meth == :method_added || meth == :singleton_method_added
  end

end

#========================================
# dump all of the annotation traces once the process if finished

Kernel.at_exit {Module.dump_type_annotations}

#========================================
