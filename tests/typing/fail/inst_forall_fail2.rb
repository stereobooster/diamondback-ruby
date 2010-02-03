
##% A<t>
class A
  
  ##% setx<self> ; self <= A<t> : t -> t
  def setx(x)
    @x = x
  end

  ##% getx<self> ; self <= A<t> : () -> t
  def getx()
    @x
  end

  ##% copy<self> ; self <= A<t> : () -> A<t>
  def copy()
    self
  end
end

a = A.new
a.setx(3)
a.getx + 4

b = a.copy
b.setx(true)
b.getx + 4
