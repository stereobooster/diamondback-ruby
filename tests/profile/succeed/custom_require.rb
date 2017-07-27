
class LoadError
end

module Kernel
  alias original_require require

  def require(path) # :nodoc:
    original_require path
  rescue LoadError
    original_require path.sub(/z$/,"")
  end
end


$: << File.expand_path("../../..", __FILE__)

require "profile/some_filez"
