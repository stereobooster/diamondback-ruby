class X; def x() end end
class Y; def y() end end

class A

  ##% f<t> : t -> X
  def f(x)
    X.new
  end

end

class B < A
  ##% f<t> : t -> Y
  def f(x)
    Y.new
  end
end


##% g: A -> A
def g(x) x end

g(A.new)
