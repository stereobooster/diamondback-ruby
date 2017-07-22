
##% A<t>
class A

  ##% add<self> ; self <= A<t> : t -> A<t>
  def add(x)
    @x = x
    self
  end

  def get()
    @x
  end

  ##% f<self> ; self <= A<t> : A<t> -> A<t>
  def f(x)
    if x.get == get
      self
    else
      fail "nope"
    end
  end
end


class B
  def z()
  end
end

a = A.new
b = B.new
a.add(b)
a.f(a)
