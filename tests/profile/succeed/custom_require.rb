
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


$: << "../.."

require "profile/some_filez"
