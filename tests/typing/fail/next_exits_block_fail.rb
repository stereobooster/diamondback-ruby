
class A
  def f()
    z = yield.foo
    "blah"
  end
end

a = A.new
z = a.f {next 3}
z.concat "blah"
