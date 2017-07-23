
require "druby/profile/collector"
require "druby/profile/require"

module DRuby
  class Profile

    def initialize(filename, append, features)

      collector = Collector.new(filename, append)

      klasses = features.map do |name|
        # for each component foo, we do the following:
        # require "druby/profile/foo"
        # DRuby::Profile::Foo(collector)
        require "druby/profile/#{name}"
        klass = DRuby::Profile.const_get name.capitalize
        klass.new(collector)
      end

      DRuby::Profile::Interceptor.enabled = true
      klasses.each {|k| k.enable}

      at_exit do
        DRuby::Profile::Interceptor.enabled = false
        klasses.each {|k| k.disable}
        collector.finalize
        # for some reason I can't figure out, writing out some yaml files 
        # changes the exit code to 1.... >:-( 
        #exit(0)
      end

    end
  end
end

