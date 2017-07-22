class C
  def to_a
    [true,false]
  end
end

class A
  def f(x,y)
    x+y
  end

  def m()
    z = C.new
    f(*z)
  end
end

A.new.m
