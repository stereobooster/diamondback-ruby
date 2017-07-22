
class A
  def f()
  end
end

class B < A
end

class C
  def g(x)
    x.f()
  end
end

C.new.g(B.new)
