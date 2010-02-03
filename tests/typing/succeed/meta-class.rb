
class A
  def f() end
end

a = A.new
class << a
  def g() end
end

a.g
a.f

