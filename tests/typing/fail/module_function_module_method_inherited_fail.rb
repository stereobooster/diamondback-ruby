
module B
  def bar() 3 end
end

module A

  include B
  module_function :bar
end

A::bar.blah
