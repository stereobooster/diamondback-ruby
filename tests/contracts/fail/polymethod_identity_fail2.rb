
class A
  ##% id<t> : t -> t
  def id(x) x.class end
end

a = A.new
a.id(3) + 4
a.id("hi") + " world"
