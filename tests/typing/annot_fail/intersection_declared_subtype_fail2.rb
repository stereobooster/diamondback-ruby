
class B
  def g() end
end

##% A <= B
class A < B
  def g(x) x end
end

class C
  ##% f : Fixnum -> Fixnum
  ##% f : B -> B
  def f(x) x end
end

c = C.new
a = A.new
c.f(a)
