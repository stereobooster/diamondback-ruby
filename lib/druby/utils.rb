
module DRuby
  module Utils

    module_function

    ## Returns the 2-tuple of the filename(string) and line
    ## number(fixnum) of most recent method in the call_stack (as
    ## returned by Kernel#caller())
    def find_caller(call_stack)
      i_caller = call_stack[0]
      if i_caller =~ /([^:]+):([0-9]+)(:.*)?/
        file = File.expand_path($1)
        line = $2.to_i
        [file, line]
      else
        $stderr.puts i_caller
        $stderr.puts "[Error] an invalid call to find_caller() #{call_stack.inspect}"
        exit(1)
      end
    end

    # checks if recv#meth_name is a method of klass
    def valid_recv?(klass, recv, meth_name) 
      meth = recv.method meth_name
      mod = nil
      case meth.inspect
      when /Method: ([^#]+)\((.*)\)#(.+)>$/
        cls = $2
      when /#<Method: #<[^>]*>\((.*)\)#(.+)>$/ # Anonymous class object
        cls = $1
      when /^#<UnboundMethod: ([^#]+)#(.+)>$/
        return false
      when /^#<Method: ([^.]+)\((.*)\)\.(.+)>$/
        cls = $2 # XXX
      when /^#<Method: ([^.]+)\.(.+)>$/ # singleton method
        return false
      when /^#<Method: ([^#\(]+)#(.+)>$/
        cls = $1
      else 
        fail "[FATAL] other #{meth.inspect}"
      end
      mod = eval(cls) 
      mod == klass 
    end

  end
end
