
require "druby/profile/interceptor"

module DRuby
  class Profile
    class Send < Interceptor

      def initialize(collector)
        super(collector, "send")
      end

      def Send.extract(recv, mname, *args)
        args[0].to_s
      end

    end
  end
end


