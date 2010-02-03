require 'test_module/a.rb'

class B
  include A
end

b = B.new
b.foo
