
class A
  def initialize(x)
    @x = x
  end

  def get() @x end
end

a = A.new(3)
a.get + 3

