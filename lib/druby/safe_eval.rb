require 'open3'
require 'yaml'
require 'shell'
require 'druby/utils'
require 'druby/contract/wrap'

module DRuby
  class Safe_eval

    # a recursive hash tbl that returns itself by default
    Self_tbl = Hash.new do Self_tbl end
    SAFE_TBL = Hash.new do Self_tbl end

    CMD = begin
            Shell.new.find_system_command "safe_eval"
          rescue Shell::Error::CommandNotFound
            fail "unable to locate safe_eval in your path"
          end
    
    class << self

      def load_profile_data()
        filename = ENV["DRUBY_PROFILE_DB"] || "druby_profile.db"
        map = YAML.load_file filename
        evals = map["eval"]
        evals.each_pair do |(file,line,eval_meth),list|
          SAFE_TBL[file] = Self_tbl unless SAFE_TBL.has_key? file
          files = SAFE_TBL[file]
          files[line] = Self_tbl unless files.has_key? line
          lines = files[line]
          list.each {|str| lines[str] = true }
        end
      end

      # TODO instance_eval, etc.. send.  require/load get/set

      def wrap_code(file,line,code,locals)
        data = [file,line,code,locals]
        new_code = nil

        Open3.popen3(CMD) do |stdin, stdout, stderr|
          YAML.dump(data,stdin)
          stdin.close
          new_code = stdout.read
          errs = stderr.read
          fail "[BUG] safe_eval instrumentation returned an error: #{errs}" unless errs.empty?
        end

        unless new_code
          fail "[BUG] safe_eval didn't return a new code at #{file}:#{line}"
        end

        return new_code
      end

      def recv_eval(me,mname,code,*loc)
        file,line = *loc
        real_file,real_line = Utils.find_caller(caller(2))
        file ||= real_file
        line ||= real_line

        if SAFE_TBL[real_file][real_line].has_key?(code)
          return me.send(mname,code,file,line)
        end

        new_code = wrap_code(real_file,real_line,code,[])
        res = me.send(mname,new_code,file,line)
        return DRuby::Contract::Wrap.wrap(real_file,real_line,code,res)
      end

      def unsafe_eval(code,scope,file,line)
        locals = __druby_eval("local_variables()",scope)
        new_code = wrap_code(file,line,code,locals)

        res = __druby_eval(new_code,scope)
        return DRuby::Contract::Wrap.wrap(file,line,code,res)
      end

      EVAL_WHITELIST = 
        [ %r{druby/contract}
        ]
         
      def real_eval(code,scope,file,line,call_file,call_line)
        if SAFE_TBL[file][line].has_key?(code) ||
            EVAL_WHITELIST.any? {|re| re =~ call_file}
          return __druby_eval(code,scope,call_file,call_line)
        else
          unsafe_eval(code,scope,file,line)
        end
      end

      def safe_eval(string,*rest)
        skip_return_frames = 2
        real_file,real_line = Utils.find_caller(caller(2))
        call_bind, call_file, call_line = rest

        call_file ||= real_file
        call_line ||= real_line

        ## If the caller passes a binding object explicitly, we use that
        if call_bind
          return real_eval(string,call_bind,real_file,real_line,call_file,call_line)
        end

        eval_result = nil
        cont = nil
        ## Otherwise we use a trace func to walk one instruction
        ## past our call to get the binding at that point, and then
        ## execute the eval.  Finally, we then return to the current
        ## continuation to return the result of the eval call
        set_trace_func proc { |event,_,_,_,binding,_|
          if skip_return_frames > 0
            if event=="return"
              skip_return_frames = skip_return_frames-1
            end
            next
          end
          set_trace_func(nil)
          eval_result = real_eval(string,binding,real_file,real_line,call_file,call_line)
          cont.call
        }
        callcc {|c| cont = c}
        return eval_result
      end

    end # << self 
  end # Safe_eval
end #DRuby

module Kernel
  alias __druby_eval eval
  def eval(*args) 
    DRuby::Safe_eval::safe_eval(*args) 
  end
end

class Object
  alias __druby_instance_eval instance_eval
  def instance_eval(*args,&blk) 
    if blk
      __druby_instance_eval.call(*args,&blk)
    else
      DRuby::Safe_eval::recv_eval(self,:__druby_instance_eval,*args) 
    end
  end
end

class Module
  alias __druby_class_eval class_eval
  def class_eval(*args,&blk) 
    if blk
      __druby_class_eval(*args,&blk)
    else
      DRuby::Safe_eval::recv_eval(self,:__druby_class_eval,*args) 
    end
  end
  alias module_eval class_eval

end

DRuby::Safe_eval.load_profile_data
