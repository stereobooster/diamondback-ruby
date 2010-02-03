
class A
  def f(x = 1, *y)
    x.foo
  end
  
end

a = A.new
a.f()
