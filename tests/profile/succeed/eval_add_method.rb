
class A
  eval "def f(x) x end"
end

A.new.f(2) + 3

