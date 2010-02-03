
class A
  class << self
    attr :ro
    attr(:rw,true)
  end
  @ro = 3
  @rw = "blah"
end

A.ro.blah
A.rw = "hi"
A.rw.concat " world"
