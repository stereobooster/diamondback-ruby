
class B
end

##% A <= B
class A < B
end

class C
  ##% f : Fixnum -> Fixnum
  ##% f : B -> B
  def f(x) x end
end

c = C.new
a = A.new
c.f(a)
