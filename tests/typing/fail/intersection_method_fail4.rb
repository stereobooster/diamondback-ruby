
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
    #x + y
  end
end

a = A.new

z = a.plus(3)
