
class A
  def foo(x)
    x + 1
  end
end

class B < A
  def foo()
    super(2) + 3
  end
end

B.new.foo
