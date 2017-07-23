
require "druby/profile/interceptor"

module DRuby
  class Profile
    class Method_missing < Interceptor

      # The method_missing collector is used by directly invoking the
      # record method, so not much to do here
      def initialize(collector) 
        super(collector, "method_missing")
      end

      def Method_missing.watch(*args)
#        STDERR.puts "DRuby MM saw: #{args.inspect}"
        super
      end

      def Method_missing.extract(recv, mname, *args)
        args[0].to_s
      end

    end
  end
end
