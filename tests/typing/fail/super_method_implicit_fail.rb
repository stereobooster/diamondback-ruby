
class C
  def foo()
    1
  end
end

class A < C
end

class B < A
  def foo(x)
    super + 3
  end
end

B.new.foo(2)
