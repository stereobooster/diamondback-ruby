
class A
end
a = A.new
A.class_eval "def f(x) x end"

a.f(2) + 3

