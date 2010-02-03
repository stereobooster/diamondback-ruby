
require "druby/profile/interceptor"

module DRuby
  class Profile
    class Reflection < Interceptor
      def initialize(collector)
        super(collector,"reflection")
      end

      def Reflection.extract(recv,mname,*args)
        #[const_name(recv), args[0].to_s]
        args[0].to_s
      end
    end
  end
end

