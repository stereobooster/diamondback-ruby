
class A
  def f() end
end

a = A.new
class << a
  def g() end
end

b = A.new
b.g

