
class A
  def initialize(x)
    x+1
  end
end

class B < A
  def initialize()
    super(2)
  end
end

B.new
