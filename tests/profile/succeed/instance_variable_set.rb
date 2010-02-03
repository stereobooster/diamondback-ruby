
class A
  def initialize() @x = nil end
end

a = A.new
x = a.instance_variable_set("@x",3)
x + 3
