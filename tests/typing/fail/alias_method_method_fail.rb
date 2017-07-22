
class A
  def x() end
  alias_method :y, :x
end

a = A.new
a.y(2)

