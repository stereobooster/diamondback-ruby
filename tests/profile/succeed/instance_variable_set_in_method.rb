class A
  def initialize() @x = nil end
  def f()
    instance_variable_set("@x",3)
  end
  def g() @x end
end

a = A.new
a.f
a.g + 3
