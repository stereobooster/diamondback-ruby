
class A
  def initialize() @x = 1 end
end

a = A.new
x = a.instance_variable_get("@x")
if false
  x.foo
end
