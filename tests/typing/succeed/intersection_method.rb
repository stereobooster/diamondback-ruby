
class B
  def foo()
  end
end

class A

  def minus()
  end

  ##% plus: (Fixnum, Fixnum) -> B
  ##% plus: (Float, Float) -> Float
  def plus(x,y)
    if x.class == Fixnum && y.class == Fixnum then
      B.new
    elsif x.class == Float && y.class == Float then
      x + y
    else
      fail "nope"
    end
  end
end

a = A.new

z = a.plus(2.3,4.1)
z + 5.0

y = a.plus(3,4)
y.foo
