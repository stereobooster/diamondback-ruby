
class A
 @x = 2
end

x = A.instance_eval do @x.foo end

x + 2

