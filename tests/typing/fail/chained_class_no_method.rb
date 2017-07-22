
class B
  def g(y)
    y.h()
  end
end

class A
  def f(x)
    b = B.new
    b.g(x)
  end
end

class C
  def cmeth
  end
end

c = C.new
a = A.new
a.f(c)
