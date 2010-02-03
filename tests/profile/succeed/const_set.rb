
class A
end

x = :X
A.const_set x, 1
A::X + 2
