
class A
  ##% fst<t,u> : (t,u) -> t
  def fst(x,y) x end
end

class B
  ##% fst<t,u> : (t,u) -> u
  def fst(x,y) y end
end


class C
  ##% g: A -> A
  def g(x) x end
end

C.new.g(A.new)
