
module A
 @x = 2
end

x = A.module_eval do @x end
x + 2

