
class B
  def x()
  end
end

class A
  def f()
    @y = B.new
    @y.y()
  end
end

A.new.f()
