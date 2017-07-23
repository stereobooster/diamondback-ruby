
require 'druby/contract'
require 'druby/utils'

module DRuby
  module Contract

    ## 
    # A class for wrapped immediate values such as fixnums
    #
    class Box
      attr_reader :val

      def initialize(x) 
        @val = x 

        meta = (class << self;self;end)

        ##
        # define this constant as a marker to later detect if we're
        # inside a box without using a method call
        #
        meta.const_set(:DRuby_Box, true)

        ##
        # Insert these binary comparison methods into the eigen class
        # of the box to perform the comparison on the original value
        #
        %w{equal? eql? == ===}.each do |m|
          meta.send(:define_method,m) do |other|
            super(Wrap.unwrap(other))
          end
        end

        ##
        # Add delegation methods to the eigen class to pass everything
        # on to the boxed value.  We don't intercept the __*__ methods
        # (confuses the interpreter), send (used by us!), and val
        # (needed to unwrap the box).  We also skip "methods" and the
        # singleton callback since immediate objects have no eigen
        # class, but we do, and we need to expose this fact for the
        # Wrap class
        #
        meths = x.methods - ["__id__","__send__","send","val", 
                             "methods", "singleton_method_added"]
         meths.each do |m|
          new_m = "def #{m}(*args,&blk) @val.send(#{m.inspect},*args,&blk) end"
          if Box.respond_to? :__druby_class_eval
            Box.__druby_class_eval new_m
          else 
            Box.class_eval new_m
          end
        end
      end
    end # Box

    
    module Wrap

      def self.unwrap(x)
        if defined?((class << x;self;end)::DRuby_Box)
          return x.val
        else 
          return x
        end
      end

      def self.wrap_block(ctx,blk)
        return nil unless blk
        proc do |*args|
          res = blk.call(*args.map {|a| DRuby::Safe_eval::Wrap.wrap_with_ctx(ctx,a)})
          next wrap_with_ctx(ctx,res)
        end
      end
      
      def self.wrap(file,line,code,x)
        ctx = Origin.new(file,line,code)
        wrap_with_ctx(ctx,x)
      end

      def self.wrap_with_ctx(ctx,x)
        case x
        when Numeric, Symbol
          x = Box.new(x)
        end

        meta = class << x; self; end
        return x if defined?(meta::DRuby_Wrapped)

        ##
        # We treat send as a trusted primitive to perform our
        # delegation and thus do not wrap it.  The __id__ method is
        # also included since Ruby complains about redefining it and
        # I'm not fully aware of the consequences of doing so
        #
        orig_methods = x.methods - ["send","__send__","__id__", "class_eval"]
        meta.const_set(:DRuby_Wrapped, orig_methods)
        meta.const_set(:DRuby_ctx, ctx)

        meta.class_eval do
          ##
          # remove all of the original methods
          #
          orig_methods.sort.each do |meth| 
            next if meth =~ /^__druby_/
            alias_method "__druby_#{meth.to_sym.to_i}", meth
            undef_method meth
          end

          def respond_to?(m)
            (class << self; self; end)::DRuby_Wrapped.include? m.to_s || super
          end

          ##
          # add our new method missing handler, obviously this must occur
          # *after* the above method removal          
          #
          def method_missing(mname,*args,&blk)
            meta = class << self; self; end
            meths = meta::DRuby_Wrapped
            ctx = meta::DRuby_ctx
            if meths.include?(mname.to_s)
              wrapped_args = args.map {|arg| 
                Wrap.wrap_with_ctx(ctx,arg)
              }
              wrapped_blk = Wrap.wrap_block(ctx,blk)
              begin 
                send("__druby_#{mname.to_sym.to_i}",*wrapped_args,&wrapped_blk)
              rescue ArgumentError => exn
                msg =  "calling #{mname} on #{self.inspect} threw an ArgumentError\n"
                msg << exn.message << "\n"
                ctx.violation msg
              rescue TypeError => exn
                msg = "calling #{self.inspect}.#{mname} with arguments "
                msg << args.map{|x|x.inspect}.join(', ')
                msg << " raised a TypeError\n"
                ctx.violation msg
              end          
            else
              file,line = Utils.find_caller(Object.send(:caller,2))
              fname = File.basename(file)
              call_stack = Object.send(:caller,3).map {|x| "        from "+x}.join "\n"
              ctx.violation "#{fname}:#{line}: undefined method `#{mname}' for #{self.inspect}:#{self.class} (NoMethodError)\n#{call_stack}"
            end
          end

          ##
          # capture any subsequently defined singleton methods and wrap them
          #
          define_method(:singleton_method_added) do |meth|
            return if meth.to_s =~ /^__druby/ || meth == :singleton_method_added
            w = "__druby_#{meth}"
            meta::DRuby_Wrapped << meth.to_s
            meta.send(:alias_method, w, meth)
            meta.send(:undef_method, meth)
          end

        end #meta.class_eval

        ##
        # capture any subsequently defined instance methods and wrap them
        # TODO: we actually need keep a list of these handlers inside the
        # class, one for each singleton
        #
        (class << x.class; self; end).send(:define_method,:method_added) do |meth|
          return if meth.to_s =~ /^__druby/
          STDOUT.puts "added instance method: #{meth.inspect}"
          w = "__druby_#{meth}"
          meta::DRuby_Wrapped << meth.to_s
          meta.send(:alias_method, w, meth.to_sym)
          meta.send(:undef_method, meth.to_sym)
        end

        ## return the final wrapped value (which is only different for
        ## immediate values)
        return x

      end # wrap_with_ctx
    end


  end # Contract
end # DRuby

module DRuby::Contract::Wrap::EqualMixin
  def equal?(other)
    super(DRuby::Contract::Wrap.unwrap(other))
  end
end

module DRuby::Contract::Wrap::EqlMixin
  def eql?(other)
    super(DRuby::Contract::Wrap.unwrap(other))
  end
end

##
# Fixnum is well behaved, it calls super when its equality operators
# fail, allowing us to intercept them using mixins, which minimizes
# overhead
#
class Fixnum
  include DRuby::Contract::Wrap::EqualMixin
  include DRuby::Contract::Wrap::EqlMixin

  ##
  # We also have to augment coerce so that a boxed value doesn't
  # degrade into using Float operations
  #
  alias_method :__druby_coerce, :coerce
  def coerce(x)
    lhs = DRuby::Contract::Wrap.unwrap(self)
    rhs = DRuby::Contract::Wrap.unwrap(x)
    lhs.__druby_coerce(rhs)
  end
end

##
# Float is almost well behaved, it calls super when equal? fails, but
# not eql?, allowing us to intercept just the first
#
class Float
  include DRuby::Contract::Wrap::EqualMixin
  alias_method :__druby_eql?, :eql?
  def eql?(other)
    __druby_eql?(DRuby::Contract::Wrap.unwrap(other))
  end
end

##
# Symbol really doesn't play nice.  It even has its own === which
# doesn't use super
#
class Symbol

  %w{== === equal? eql?}.each do |meth|
    old_meth = :"__druby_eq_#{meth.to_sym.to_i}"
    alias_method old_meth, meth
    define_method meth do |other|
      send(old_meth,DRuby::Contract::Wrap.unwrap(other))
    end
  end
end

##
# Similarly, const_set must take an actual symbol, no public method is
# called to test this, only an internal check
#
class Module
  alias_method :__druby_const_set, :const_set
  def const_set(sym,val)
    __druby_const_set(DRuby::Contract::Wrap.unwrap(sym),val)
  end
end


