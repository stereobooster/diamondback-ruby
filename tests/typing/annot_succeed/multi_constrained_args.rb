
##% f<t,u> ; t <= A, u <= A : (t,u) -> u
def f(x,y) 
  y
end

class A
  def a() self end
end

class B < A
  def b() self end
end

class C < A
  def c() self end
end

b = B.new
c = C.new
f(b,c).c
