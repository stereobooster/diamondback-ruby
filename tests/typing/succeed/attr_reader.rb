
class A
  attr_reader :x
  def setx(x) @x = x end
end


a = A.new
a.setx(2)
a.x + 3
