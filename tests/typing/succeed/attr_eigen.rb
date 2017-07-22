
class A
  class << self
    attr :ro
    attr(:rw,true)
  end
  @ro = 3
  @rw = "blah"
end

A.ro + 4
A.rw = "hi"
A.rw.concat " world"
