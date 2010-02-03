
class A
  class << self
    attr :ro
    attr(:rw,true)
  end
  @ro = 3
  @rw = "blah"
end

A.rw - 3
