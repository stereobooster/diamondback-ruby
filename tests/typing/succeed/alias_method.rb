
class A
  def x() end
  alias :y :x
end

a = A.new
a.y

