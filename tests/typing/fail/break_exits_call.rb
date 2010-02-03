class A
  def f()
    yield
    nil
  end
end

a = A.new
z = a.f {break "blah"}
z + 4
