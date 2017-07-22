
class A
end
a = A.new
a.instance_eval "def f(x) x end"
a.f(2) + 3

