
class A
  def A.x() end
  class << self
    alias :y :x
  end
end

A.y

