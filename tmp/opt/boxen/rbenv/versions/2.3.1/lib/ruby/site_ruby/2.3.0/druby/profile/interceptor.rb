require 'druby/utils'

module DRuby
  class Profile

    ## A class for intercepting Ruby methods and storing information
    ## about their arguments are return values
    class Interceptor

      class << self
        attr_accessor :enabled
      end
      @enabled = false

      ## collector -- an instance of DRuby::Profile::Collector 
      ## 
      ## name -- a string representing the name of the componenet
      ## which will be used as the key for any data stored in the
      ## collector
      def initialize(collector, name)
        @col_hash = collector.register name
        self.class.const_set(:Singleton,self)
        @klasses = {}
        @frame_skips = {}
        @enabled = false
      end

      def Interceptor.const_name(e)
        unless Class === e || Module === e 
          STDERR.puts "can't handle modifications to singleton class yet"
          exit(1)
        end

        name = e.name

        return [:class, name] if name != ""
          
        case e.inspect
        when /^\#<Class:([^\d][^>]+)>$/
          [:singleton, $1]
        when /^\#<Class:0x.*>$/
          STDERR.puts "handle immeidate class object"
          exit(1)
        else 
          STDERR.puts "unknown class kind: #{e.inspect}"
          exit(1)
        end
      end

      def Interceptor.watch(filename, lineno, recv, mname, *args)
#        if enabled
          mname = mname.to_s
          #return unless Utils.valid_recv?(Receivers[mname], recv, mname)
          self::Singleton.store(filename,lineno,mname,extract(recv,mname,*args))
#        end
      end

      def add_frame_skip(klass,mname)
        @frame_skips[klass][mname] += 2
      end

      ## Registers [blk] as an interceptor for the method klass#mname.
      ## This block is passed original method (as a Method) followed
      ## by the actual arguments of the call.  The block should
      ## perform any computations itself, including storing any
      ## results and calling the original method if necessary, and
      ## return the corresponding return value.
      def intercept(klass,mname,&blk)
        unless klass.class == Class || klass.class == Module
          fail "first arg to intercept_method should be a class or module"
        end
        mname = mname.to_sym
        old_method = klass.instance_method mname
        # old_method is unbound at this point

#        new_meth = instrument_method(klass,mname,old_method,blk)
#        klass.class_eval {define_method mname, &new_meth}
        @klasses[klass] = {} unless @klasses.key? klass 
        @klasses[klass][mname] = [old_method,blk]
        @frame_skips[klass] = {} unless @frame_skips.key? klass
        @frame_skips[klass][mname] = 1
      end

      ## A simply form than [intercept].  The block again receives the
      ## original method and the actual arguments, but should not
      ## invoke the original method.  Instead, it is expected to
      ## return a single value, representing the value to be stored in
      ## the collector, and the original method will be called
      ## regardless.
      def intercept_args(klass,mname,&blk)
        unless klass.class == Class || klass.class == Module
          fail "first arg to intercept_method should be a class or module"
        end
        mname = mname.to_sym
        unbound_method = klass.instance_method mname
        
        wrap = lambda do |old,*args|
          record_loc(klass,mname,blk.call(*args))
          old.call(*args)
        end

        @klasses[klass] = {} unless @klasses.key? klass 
        @klasses[klass][mname] = [unbound_method,wrap]
#        new_meth = instrument_method(klass,mname,unbound_method,wrap)
#        klass.class_eval {define_method mname, &new_meth}
      end

      ## append the data to the list of stored values
      def store(file,line,method,v)
        key = [file,line,method.to_s]
        if @col_hash.key? key then
          (@col_hash[key] << v) unless @col_hash[key].include? v
        else
          @col_hash[key] = [v] 
        end
      end

      def callee(skip,method)
        callers = caller(1+skip)
        if callers.any? {|s| s =~ /\(eval\)/} then
          STDERR.puts "warning #{method} used inside of an eval"
          return
        end
        file,line = Utils.find_caller(callers)
      end
      
      ## Add the data to the collector
      def record_loc(klass,method,v)
        skip = @frame_skips[klass][method]
        file,line = callee(skip,method)
        store(file,line,method,v)
      end

      ## instrument_method takes the following arguments:
      ##   method_name - the original method name as a symbol
      ##   unbound_orig_method - an UnboundMethod for the original method
      ##   inst_code - a proc that should be called instead
      ##
      ## It then builds a new proc that represents the body of the
      ## intercepted method.  This proc calls inst_code, passing it a
      ## (bound) Method object representing the original method, and
      ## the arguments to the method.  It then stores the resulting
      ## data into the current instance's collector, and executes the
      ## continuation of the actual method
      def instrument_method(klass,method_name,unbound_orig_method,inst_code)        
        lambda do |*args|
          bound_method = unbound_orig_method.bind(self)
#          if ::DRuby::Profile::Interceptor.enabled then
            inst_code.call(bound_method,*args)
#          else
#            bound_method.call(*args)
#          end
        end
      end

      ## Intercept each method using define_method with the block
      ## regestered with one of the intercept* methods above.  We do
      ## this under class_eval since define_method is private
      def enable()
        fail "#{self} already enabled" if @enabled
        @klasses.each_pair do |k,methods|
          methods.each_pair do |method_sym, (orig,blk)|
            new_meth = instrument_method(k,method_sym,orig,blk)
            k.class_eval do
              define_method method_sym, &new_meth
            end
           end
         end
        @enabled = true
       end
 
      ## Restore the methods to their original state
      def disable()
        fail "#{self} already disabled" unless @enabled
        @klasses.each_pair do |k,methods|
          methods.each_pair do |method_sym, (old,blk)|
            bound = old.bind(k)
            k.class_eval {define_method method_sym, &bound.to_proc}
          end
        end
        @enabled = false
      end


    end
  end
end
