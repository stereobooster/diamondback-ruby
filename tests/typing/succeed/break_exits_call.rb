
class A
  def f()
    yield
    nil
  end
end

a = A.new
z = a.f {break 3}
z + 4
