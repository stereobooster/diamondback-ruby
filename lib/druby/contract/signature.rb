require 'druby/contract'

module DRuby
  module Contract

    ##
    # A global registry for storing the static types of a method
    #
    class Registry
      @gradual_classes = {}
      def self.register(clazz, meth, sig)
        @gradual_classes[clazz] = {} unless @gradual_classes[clazz]
        @gradual_classes[clazz][meth] = sig
      end
      
      def self.get_sig(name,m)
        if meths = @gradual_classes[name]
        then return meths[m]
        else return nil
        end
      end
    end

    ##
    # Base class for all contacts
    #
    class BaseContract
      @@gamma = {}
      def gamma() @@gamma end
      def gamma=(g) @@gamma = g end

      def visit(v)
        raise "Implement visit in #{self.class}"
      end

      def static_type(v)
        raise "Implement static_type in #{self.class}"
      end

      def build_ctx_string(msg,ctx)
        s = "[ERROR] " << msg
        ctx.each {|m| s << "\n  " << m}
        s << "\n"
      end

      def Violation(ctx,msg)
        s = build_ctx_string(msg, ctx)
        throw :ContractViolation, [:err,s]
      end
    end

    class TypeVisitor
      def initialize(&blk)
        @block = blk
      end
      def visit_type(t)
        @block.call t
      end
    end

    ##
    # A Paramter contract ensures that the number of actual arguments
    # is consisent with the static number (equal up to differences in
    # optional and vararg arguments).  It also ensures that each
    # actual argument's type is consistent with the static type of the
    # formal argument
    #
    class Params < BaseContract
      def initialize(reqs,opts = [],var = nil)
        @required = reqs
        @opts = opts
        @varargs = var
      end

      def visit(v)
        @required.each {|t| v.visit_type t}
        @opts.each {|t| v.visit_type t}
        v.visit_type @varargs if @varargs
      end

      def static_type
        args = []
        args += @required.map{|t|t.static_type}
        args += @opts.map{|t| "?#{t.static_type}"}
        if @varargs
          args << "*#{@varargs.static_type}"
        end
        args.join(", ")
      end

      def assert_contract(ctx_,actuals)
        ctx = ["checking parameter list"] + ctx_
        assert_length(ctx,actuals.length)

        req_actuals = actuals[0...(@required.length)]
        opt_actuals = actuals[(@required.length)...(@required.length+@opts.length)]
        var_actuals = actuals[(@required.length+@opts.length)...(actuals.length)]

        num = 0
        ctx = ["checking required parameters list"] + ctx_
        @required.zip(req_actuals).each do |(req,act)| 
          num += 1;
          ctx = ["checking parameter #{num}"] + ctx_
          req.assert_contract(ctx,act)
        end
        ctx = ["checking optional parameters list"] + ctx_
        @opts.zip(opt_actuals).each do |(opt,act)| 
          num += 1;
          ctx = ["checking parameter #{num}"] + ctx_
          opt.assert_contract(ctx,act) if (opt && act)
        end
        ctx = ["checking variable parameters list"] + ctx_
        if @varargs
          var_actuals.each do |act|
            num += 1;
            ctx = ["checking parameter #{num}"] + ctx_
            @varargs.assert_contract(ctx,act)
          end
        end

      end

      def assert_length(ctx,num_actuals)
        if num_actuals < @required.length
          Violation(ctx,"exptected at least %s formal parameters, got %d actuals" %
                    [@required.length, num_actuals])
        end
        return true if @varargs
        if num_actuals > @required.length+@opts.length
          Violation(ctx,"exptected at most %s formal parameters, got %d actuals" %
                    [(@required.length+@opts.length), num_actuals])
    end
      end
    end

    ##
    # A UnionType is simply a list of potential types to match
    # against.  This contract is satisfied as long as at least one
    # type in the list of union types does not cause a contract
    # violation
    #
    class UnionType < BaseContract
      def initialize(tlist)
        @types = tlist
      end

      def visit(v)
        @types.each {|t| v.visit_type t}
      end

      def assert_contract(ctx,tprime)
        ctx = ["Union Type"] + ctx
        return if tprime.nil?
        unless @types.any? do |t| 
            (catch :ContractViolation do
               t.assert_contract(ctx,tprime)
               true
             end) == true
          end
          Violation(ctx,"contract type error: #{tprime.class.name} is not a member of this union type")
        end
      end

    end

    ##
    # A class type satisfies its contract if the object is a subclass
    # of the static type as determined by Module#===.  TODO: we should
    # really be doing a structural test here
    #
    class ClassType < BaseContract
      def initialize(t)
        @cname = t
      end

      def visit(v)
        v.visit_type(self)
      end

      def assert_contract(ctx,tprime)
        clazz = eval @cname
        unless tprime.nil?
          unless clazz === tprime
            Violation(ctx,"contract type error: expected #{clazz.name}, got #{tprime.class.name}")
          end
        end
      end
      def static_type() @cname end
    end

    ##
    # A polymorphic variable contract checks for paramtricity
    # violations.  A single instance of this class is intended to be
    # shared throughout a class or method signature.  Thus, the first
    # value to be checked against a PolyVar verifies its bound, and
    # subsequent contract checks ensure that the same type is used
    # consistently.
    #
    class PolyBinder < BaseContract
      attr_reader :name

      def initialize(name,pos,bound = nil)
        @name = name
        @pos = pos
        @bound = bound
      end
        
      def assert_contract(ctx,val)
        if @type
          unless val.class.equal? @type
            msg =  "parametricity violation: expected #{@type.name}, "
            msg << "got #{val.class.name}"
            Violation(ctx,msg)
          end
        else
          @bound.assert_contract(ctx,val) if @bound
          @type = val.class
        end
      end

    end

    ##
    #
    #
    class PolyVar < BaseContract
      attr_reader :name
      attr_writer :binder
      def initialize(name)
        @name = name
        @binder = nil
      end

      def assert_contract(ctx,t)
        raise "DRUBY BUG, polyvar #{@name.inspect} not in gamma" unless gamma.include? @name
        gamma[@name].assert_contract(ctx,t)
      end

      def visit(v)
        v.visit_type self
      end
      
      def static_type
        @name
      end
    end

    ##
    # An ObjectType is a structural type. TODO
    #
    class ObjectType < BaseContract
      def initialize(fields,methods)
        @fields = fields
        @methods = methods
      end
      
      def assert_contract(ctx,tprime)
#        Violation(ctx,"Unsupported contract type: Object Type")
      end
    end


    ##
    # A polymorphic method is a method signature with at least one
    # quantified type variable
    #
    class PolyMethod < BaseContract

      def initialize(binders,method,pos)
        @binders = binders
        @method = method
        @pos = pos
      end
      
      def assert_contract(actuals,blk,&meth)
        base_ctx = [@pos.to_s,*caller]

        old_gamma = gamma
        @binders.each do |(var,bound)| 
          gamma[var] = PolyBinder.new(var,@pos,bound)
        end

        result = catch :ContractViolation do
          @method.assert_contract(actuals,blk,base_ctx,&meth)
        end

        gamma = old_gamma
        err,msg = result
        if err == :err
          puts msg
          exit 1
        else
          result
        end
      end
    end

    ##
    # A monomorphic method is a method with no quantified type
    # variables
    #
    class MonoMethod < BaseContract
      attr_reader :params, :block, :ret

      def initialize(name,args,blk,ret,pos)
        @name = name
        @pos = pos
        @params = args
        @block = blk
        @ret = ret
      end

      def visit(v)
        @params.each {|t| v.visit_type t}
        @block.visit v
        v.visit_type @ret
      end

      def assert_params(actuals,base_ctx)
        ctx = ["checking parameters"] + base_ctx
        @params.assert_contract(ctx,actuals)
      end

      def assert_return(result,base_ctx)
        ctx = ["verifying return type"] + base_ctx
        @ret.assert_contract(ctx,result)
      end

      def assert_contract(actuals,blk,base_ctx=nil,&meth)
        base_ctx ||= [@pos.to_s,*caller]
        res = catch :ContractViolation do
          assert_params(actuals,base_ctx)

          ctx = ["verifying block signature"] + base_ctx
          blk2 = if @block then @block.project(ctx,blk) else blk end

          result = meth.call(actuals,blk2)
          assert_return(result,base_ctx)
          result
        end
        err,msg = res
        if err == :err
          puts msg
          exit 1
        end
        res
      end
    end

    ##
    # An intersection method is a list of monomorphic or polymorphic
    # methods.
    #
    class InterMethod < BaseContract
      def initialize(name, funcs, pos)
        @name = name
        @funcs = funcs
        @pos = pos
      end

      def visit(v)
        @funcs.each {|m| m.visit v}
      end

      def invalid_return(valid,actuals)
        actual_str = actuals.map{|t|t.class.name}.join(', ')
        msg = "Invalid return type\n"
        msg << "  Expecting one of the following signatures:\n"
        valid.each do |(blk,ret)|
          msg << "    #{@name}: (#{actual_str})"
          msg << " {#{blk.static_type}}" if blk 
          msg << " -> #{ret.static_type}\n"
        end
        msg << "  Expecting a return type of #{valid.map{|(x,y)|y.static_type}.join(' or ')}"
      end

      def invalid_block_arg(valid,actuals,call_src)
        /(.*):in `/ =~ call_src
        src = $1
        msg = "Invalid block argument type\n"
        msg << "  The block passed to the #{@name} method expects one of the following signatures:\n"
        valid.each do |(blk,*)|
          msg << "    #{blk.static_type}\n"
        end
        msg << "  However, the call at #{src} passed the arguments "
        msg << "#{actuals.map{|t| "#{t.inspect}:#{t.class.name}"}.join(', ')}"
      end
      
      def assert_contract(actuals,blk,&meth)
        stack = caller()
        res = catch :ContractViolation do
          ctx = [@pos.to_s,*stack]
          valid = []
          
          @funcs.each do |method|
            catch :ContractViolation do
              method.params.assert_contract(ctx,actuals)
              valid.push [method.block,method.ret]
              next
            end
          end
          blk_ctx = ctx.clone
          blk2 = Proc.new do |*blk_actuals|
            if valid.empty? 
              Violation(ctx,"all members of intersection type failed in before block test")
            end

            current = valid.reject do |(fblk,fret)|
              unless fblk
                next(true) 
              end
              res = catch :ContractViolation do
                fblk.args.assert_contract(blk_ctx,blk_actuals) 
                false
              end
              if res == false then next(false)
              else next(true)
              end
            end
            if current.empty? 
              Violation(ctx,invalid_block_arg(valid,blk_actuals,caller[0]))
            end
            valid = current

            r = blk.call(*blk_actuals)
            current = valid.reject do |(fblk,fret)|
              next(true) unless fblk
              err, m = catch :ContractViolation do
                fblk.ret.assert_contract(ctx,r) 
              end
              next(true) if err == :err 
              next false
            end
            if current.empty? 
              Violation(ctx,"all members of intersection type failed in block return test")
            end
            valid = current
            r
          end if blk

          begin
            r = meth.call(actuals,blk2)
          rescue ArgumentError => exn
            msg = "Untyped method did not accept #{actuals.length} arguments (given: #{actuals.inspect})"
            Violation(ctx, msg)
          rescue NoMethodError => exn
            msg = "Untyped method threw NoMethodError in body (arguments: #{actuals.inspect})\n"
            msg << "  #{exn.message}"
            Violation(ctx,msg)
          end
          ctx = ["Method returned the type #{r.class.name}"] + ctx
          current = valid.reject do |(fblk,fret)|
            err,m = catch :ContractViolation do
              fret.assert_contract(ctx,r) 
            end
            next(true) if err == :err
            next false
          end

          if current.empty? 
            Violation(ctx,invalid_return(valid,actuals))
          end
          valid = current
          r

        end
        err,msg = res
        if err == :err
          puts msg
          exit 1
        end
        res
      end

    end

    ##
    # A block contract simply checks its arguments and return values
    #
    class Block < BaseContract
      attr_reader :args, :ret
      def initialize(args,ret)
        @args = args
        @ret = ret
      end

      def visit(v)
        @args.each {|t| v.visit_type t}
        v.visit_type @ret
      end

      def static_type
        "(#{@args.static_type}) -> #{@ret.static_type}"
      end

      def project(base_ctx,blk)
        Proc.new do |*actuals|
          Violation(base_ctx,"block was not provided") unless blk
          ctx = ["verify block arguments"] + base_ctx
          @args.assert_contract(ctx,actuals)
          r = blk.call(*actuals)
          ctx = ["verify block return"] + base_ctx
          @ret.assert_contract(ctx,r)
          r
        end
      end
    end
  end
end

class Module
  @@inserted = {}

  alias :old_include :include
  def include(*args)
    #    args.each {|m| puts "#{self.name} include #{m}"}
    old_include(*args)
  end
  
  alias :old_added :method_added
  def method_added(meth)
    if DRuby::Contract::Registry.get_sig(self.name,meth)
      @@inserted[self.name] = {} unless @@inserted[self.name]
      if @@inserted[self.name][meth] && !@inserting_cast
        #        puts "Already instrumented: #{self.name} #{meth}"
      end
      @@inserted[self.name][meth] = 1
      insert_cast(meth) unless meth == :initialize
    end
    old_added(meth)
  end
  @@count = 1
  def insert_cast(m)
    if @inserting_cast then return
    else 
      @inserting_cast = true
      @@count += 1
      #puts "instrumenting #{self.name} #{m}"
      if /^[a-zA-Z_]*$/ =~ m.to_s
        orig = :"untyped_#{m}"
      else
        orig = :"untyped_#{@@count}"
      end
      self.class_eval do 
        define_method(orig,instance_method(m))
        if /^([^:]+):(\d+)/ =~ caller[3]
          file = $1
          line = $2.to_i
        else
          file = __FILE__
          line = __LINE__+3
        end
        eval <<-EOF, binding(), file, line
        def #{m}(*args,&blk)
          sig = DRuby::Contract::Registry.get_sig("#{self.name}",#{m.inspect})
                                           sig.assert_contract(args,blk) do |args,blk| 
                                             if args then
                                               #{orig}(*args,&blk) 
                                             else
                                               #{orig}(&blk) 
                                             end
                                           end
                                         end
        EOF
      end
    @inserting_cast = false
    end
  end
end
