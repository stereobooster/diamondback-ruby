
class A
  def f(x) x end
end

a = A.new
z = a.send(:f, 3)
if false
  z.foo
end
