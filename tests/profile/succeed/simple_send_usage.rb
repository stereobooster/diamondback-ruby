
class A
  def f(x) x end
end

a = A.new
a.send(:f, 3)
