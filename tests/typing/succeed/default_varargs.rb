
class A
  def f(x = 1, *y)
    x + 2
  end
  
end

a = A.new
a.f(1,2,3)
a.f(4)
a.f()
