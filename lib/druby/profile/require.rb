
require "druby/profile/interceptor"

module DRuby
  class Profile
    class Require < Interceptor
      
      class << self
        attr_accessor :track_frames
      end
      self.track_frames = false

      def build_proc(is_require,klass,method)
        Proc.new do |orig_method,*args|
          req_file = args[0]

          skip = @frame_skips[klass][method]
          file,line = callee(2+skip,method)

          cont = 
            begin
              res = orig_method.call(*args)
              Proc.new {res}
            rescue Object => e
              res = e.class.to_s
              Proc.new {raise e}
            end
          store(file,line,method,[is_require,req_file,res,$:.clone])
          cont.call()
        end
      end

      def store(file,line,method,v)
#        STDERR.puts "store require: #{file} #{line} #{v[0]} #{v[1]} #{v[2]}"
        super
      end

      def initialize(collector)
        super(collector, "require")

        kernel_s = class << Kernel;self;end
        intercept Kernel, :require, &build_proc(true,Kernel,:require)
#        intercept kernel_s, :require, &build_proc(true)
        intercept Kernel, :load, &build_proc(false,Kernel,:load)
#        intercept kernel_s, :load, &build_proc(false)
      end

      def enable()
        super
        Require.track_frames = true
      end

      def disable()
        Require.track_frames = false
        super
      end
    end

  end
end


class ::Module
  alias orig_method_added method_added
  private
  def method_added(meth)
    if DRuby::Profile::Require.track_frames then
      if self == Kernel 
        if meth == :require then
          DRuby::Profile::Require::Singleton.add_frame_skip(Kernel,:require)
        elsif meth == :load then
          DRuby::Profile::Require::Singleton.add_frame_skip(Kernel,:load)
        end
      end
    end
    orig_method_added(meth)
  end
end

