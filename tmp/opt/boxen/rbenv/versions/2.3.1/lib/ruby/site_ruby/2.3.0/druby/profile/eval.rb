require "druby/utils"
  
module DRuby
  class Profile
    class Eval < Interceptor

      def initialize(collector)
        super(collector,"eval")
      end

      Receivers = {
        "eval" => Kernel, 
        "instance_eval" => Kernel,
        "module_eval" => Module,
        "class_eval" => Module
      }

      def Eval.record_eval(filename, lineno, recv, mname, *args, &blk)
        mname = mname.to_s
        return unless Utils.valid_recv?(Receivers[mname], recv, mname)
        return if (not blk.nil?) || args.empty?
        self::Singleton.store(filename,lineno,mname,args[0])
      end

    end
  end
end

