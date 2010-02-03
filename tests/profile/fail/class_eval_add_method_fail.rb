
class A
end
a = A.new
A.class_eval "def f(x) x end"
if false
  a.f() + 3
end

