
class A
 @x = 2
end

x = A.class_eval do @x end
x.foo

