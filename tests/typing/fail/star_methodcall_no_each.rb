class C
end

class A
  def f(x,y)
  end
end

class B 
  def m()
    z = C.new
    f(*z)
  end
end

B.new.m
