
class A
  eval "def f(x) x end"
end

if false
  A.new.f() + 3
end

