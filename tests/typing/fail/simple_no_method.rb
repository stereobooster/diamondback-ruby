

class A
  def f(x)
    x.g
  end
end

class B
end

A.new.f(B.new)
