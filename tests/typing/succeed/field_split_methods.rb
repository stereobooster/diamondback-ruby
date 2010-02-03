class B
  def x()
  end
end

class A
  def f()
    @y = B.new
  end

  def g()
    @y.x()
  end
end
