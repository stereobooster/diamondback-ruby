

class A
  attr_accessor :x1, :x2
end

a = A.new
a.x1 = 3
a.x2 = a.x1
a.x2.foo
