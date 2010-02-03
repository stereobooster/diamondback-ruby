
##% A<t> 
class A
  def initialize(x) @x = x end

  ##% f : () -> t
  def f() @x end

  ##% g<v> : (v) -> A<v>
  def g(x) A.new(x) end
end

z = A.new("hi").g(true)
z.f + 2
