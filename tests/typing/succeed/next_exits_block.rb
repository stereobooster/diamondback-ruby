
class A
  def f()
    z = yield + 2
    "blah"
  end
end

a = A.new
z = a.f {next 3}
z.concat "blah"
