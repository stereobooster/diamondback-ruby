
class A
  ##% id<t> : t -> t
  def id(x) x end
end

a = A.new
a.id(3) + 4
a.id("hi") + " world"
