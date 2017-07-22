class A
  def f(x=self)
    x.g
  end
  def g
  end
end

A.new.f
