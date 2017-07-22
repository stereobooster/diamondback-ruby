
class A
  X = 1
end

x = :X
one = A.const_get x
if false
  one.foo
end
