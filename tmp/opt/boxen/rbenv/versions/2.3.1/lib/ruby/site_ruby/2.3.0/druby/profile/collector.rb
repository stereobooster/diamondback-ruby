
module DRuby
  class Profile

    class Collector
      def initialize(file, append)
        @entries = {}
        @file = file
        @append = append
      end

      def register(name)
        fail "tried to add collection #{name} twice" if @entries.key? name
        @entries[name] = {}
      end

      def finalize
        require 'yaml'
        if @append && File.exists?(@file) 
          map = YAML.load_file(@file) 
          @entries.each_pair do |kind, data|
            map[kind] = data
          end
          open(@file, "w") do |f| f.puts map.to_yaml end
        else
          open(@file, "w") do |f| f.puts @entries.to_yaml end
        end
      end
    end
  end
end
