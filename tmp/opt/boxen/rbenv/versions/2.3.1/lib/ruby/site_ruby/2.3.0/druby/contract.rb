
$DRUBY_ABORT_ON_CONTRACT=true

module DRuby
  module Contract
    class ContractViolation < Exception
    end

    class Origin
      def initialize(file,line,code)
        @file = file
        @line = line
        @code = code
      end

      def violation(msg)
        fname = File.basename(@file)
        err = "[ERROR] Runtime Contract Violation\n"
        if @code
          err << "The eval at #{fname}:#{@line} was passed the following string\n"
          @code.chomp.each_line do |l|
            err << "  #{l}"
          end
          err << "\n"
          err << "Subsequently, the following error occured:\n"
        else
          err << "Unchecked code from #{fname}:#{@line} caused the following error:\n"
        end
        msg.chomp.each_line do |l|
          err << "  " << l
        end
        err << "\n"
        if $DRUBY_ABORT_ON_CONTRACT
          err << (caller(2).map {|x| "  from "+x}.join "\n")
          $stderr.puts err
          exit(1)
        else
          raise ContractViolation.new(err)
        end
      end
      
      def to_s
        "In file #{@file}, line #{@line}"
      end
    end
  end
end
