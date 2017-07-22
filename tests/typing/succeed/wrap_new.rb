
class A
  def A.new2() new(2) end
  def initialize(x) @x = x end
  def x() @x end
  def A.g() 3 end
end

A.new2.x + 3
