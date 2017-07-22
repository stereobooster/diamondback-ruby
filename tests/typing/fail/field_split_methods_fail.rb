class B
  def x()
  end
end

class A
  def f()
    @y = B.new
  end

  def g()
    @y.y()
  end
end

a = A.new
a.f
a.g
