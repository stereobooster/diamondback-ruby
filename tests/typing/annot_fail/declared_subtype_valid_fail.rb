
class B
  def g()
  end

  def f(x) x + 2 end
end

##% A <= B
class A < B

  def g(x)
    2+3
    nil
  end

  def f(x) x * 4 end
end


