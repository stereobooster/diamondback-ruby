
class A
  def __is_a() end
end

a = A.new

x, y = 1,a

y.__is_a()
x.foo
