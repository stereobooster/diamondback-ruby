
class A

  def h(x)
    g(x)
  end
  def g(x)
    f(x)
  end
  def f(x)
    h(x)
  end
end

def go
  A.new.f(30)
end
