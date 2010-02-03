class A
  def f(x=self)
    x.g
  end
  def A.g
  end
end

A.new.f
