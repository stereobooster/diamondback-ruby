
class A
  def getx() @x=2 end
end

a = A.new
a.instance_variable_set("@x",3)
x = a.getx
if false
  x.foo
end
