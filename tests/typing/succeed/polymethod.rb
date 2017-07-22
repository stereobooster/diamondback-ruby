
class A
  def f() end
end

class B
  def g() end
end

##% m<u>: u -> u
def m(x) x end


a = A.new
b = B.new
m(a).f()
m(b).g()
