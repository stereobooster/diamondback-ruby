require "druby/utils"
# require 'digest/md5' 

## This file contains helper methods that DRuby inserts during program
## transformations.  This code is trusted by DRuby and not analyzed
## along with the user's code.  As such, it should limit any calls to
## require/load itself, and should in general try to be kept small.

#
# simualtes `...`
#
def __backtick(*args) 
  return `#{args[0]}`
end

module DRuby
  class Profile
    class Runtime

      class Mining
        @@num_of_meths = 0
        def self.run_test?()
          return true if not ENV["DRUBY_COVERAGE"]
          result = (@@num_of_meths < ENV["DRUBY_COVERAGE"].to_i)
          @@num_of_meths += 1
          return result
        end
      end

      Require = method(:require)
      Load = method(:load)
      NativeExts = [".so",".o"]
      class << self

        def enabled()
          (defined? DRuby::Profile::Interceptor.enabled) &&
            DRuby::Profile::Interceptor.enabled
        end

        # instrumented require
        def safe_load(orig, *args) 
          Load.call(*args) 
        end
        
        # instrumented require 
        def safe_require(orig, mapped)
          if $".include? orig then false
          else
            # if orig == mapped, then the require call will add it (or the
            # require call will fail)
            $" << orig if orig != mapped
            Require.call(mapped)
          end
        end

        # A require statement that always throughs an exception
        def exn_require(name) 
          Require.call(name) 
          STDERR.puts "exn_require was supposed to throw an exception"
          STDERR.puts "stack: #{caller()}"
          exit(1)
        end

        # A require statement that always throughs an exception
        def exn_load(*args) 
          Load.call(*args) 
          STDERR.puts "exn_load was supposed to throw an exception"
          STDERR.puts "stack: #{caller()}"
          exit(1)
        end

        def dead_method(mname,*args)
          $stderr.puts "[ERROR] dead method #{mname} was called, aborting"
          exit(1)
        end

        def dead_require(recv, mname, *args)

          if not Utils.is_recv_valid?(Kernel, recv, mname)
            return recv.send(mname,*args)
          end

          $stderr.puts "[ERROR] dead #{mname} was actually called?"
          $stderr.puts "args: #{args}"
          $stderr.puts(caller()[0])
          $stderr.puts "\n\n"

          exit(1)
          #Require.call(args[0])

        end

      end
    end
  end
end

module Kernel
  def require(file)
    dpr = DRuby::Profile::Runtime

    if dpr.enabled && dpr::NativeExts.all? {|e| File.extname(file) != e}
      $stderr.puts "[WARNING] uninstrumented require #{file}"
      $stderr.puts(caller[0])
      # exit(1)
    end
    DRuby::Profile::Runtime::Require.call(file)
  end

  def load(file,b=false)
    dpr = DRuby::Profile::Runtime

    if dpr.enabled && dpr.NativeExts.all? {|e| File.extname(file) != e}
      $stderr.puts "[WARNING] uninstrumented load #{file}"
      $stderr.puts(caller[0])
      # exit(1)
    end
    DRuby::Profile::Runtime::Load.call(file,b)
  end

end

