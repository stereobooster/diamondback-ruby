

class B
  def g() end
end

class A
  def f()
    B.new
  end
end

a = A.new
a.f.g
    
