class A
  def initialize() @x = 4 end
  def f()
    instance_variable_set("@x","blah")
  end
  def g() @x end
end

a = A.new
a.g - 3
a.f
