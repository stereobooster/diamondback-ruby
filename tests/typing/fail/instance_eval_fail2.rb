
class A
  def set(y)
    @x = y
  end
end

class B
  def g()
    @x = "hi"
    a = A.new
    a.set(8)
    z = a.instance_eval {@x-4}
    z.foo
  end
end

B.new.g
