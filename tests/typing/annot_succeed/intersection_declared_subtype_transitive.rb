
class C
end

##% B <= C
class B
end

##% A <= B
class A < B
end

class D
  ##% f : Fixnum -> Fixnum
  ##% f : C -> C
  def f(x) x end
end

c = D.new
a = A.new
c.f(a)
