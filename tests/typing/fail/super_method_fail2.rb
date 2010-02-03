
class A
end

class B < A
  def foo(x)
    super(x) + 3
  end
end

B.new.foo(2)
