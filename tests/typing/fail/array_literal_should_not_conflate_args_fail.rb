
class A
  def f();end
end

class B
  def f();end
  def g();end
end

z = [A.new,B.new]
z[0].g
