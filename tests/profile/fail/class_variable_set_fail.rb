
class A
  def initialize() @x = nil end
end

a = A.new
x = A.instance_eval {class_variable_set("@@x",3)}
if false
  x.foo
end
