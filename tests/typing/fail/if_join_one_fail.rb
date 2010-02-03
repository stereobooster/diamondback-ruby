
class A
  def x() end
  def y() end
end

class B
  def y() end
  def z() end
end

class C 
  def m()
    if not false
      x = A.new
    else
      x = B.new
    end
    x.z
  end
end

C.new.m
