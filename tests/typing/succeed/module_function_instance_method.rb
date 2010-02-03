
module M
  def foo() 3 end
  module_function :foo
end

class A
  include M
  def bar() foo end
end

A.new.bar + 4
