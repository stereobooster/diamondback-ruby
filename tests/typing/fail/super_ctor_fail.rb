
class A
  def initialize()
  end
end

class B < A
  def initialize()
    super(2)
  end
end

B.new
