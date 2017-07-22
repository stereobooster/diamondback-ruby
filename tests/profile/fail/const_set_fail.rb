
class A
end

x = :X
A.const_set x, 1
if false
  A::X.foo
end
