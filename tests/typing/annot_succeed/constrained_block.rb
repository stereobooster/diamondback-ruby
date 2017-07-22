
class A
  ##% f<t> : () {Fixnum -> t} -> t
  def f()
    yield(3)
  end
end

a = A.new
a.f() {|x| x + 3}
a.f() {|x| "hi"}
