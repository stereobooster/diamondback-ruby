
class A
  def a() end
end

##% x: String -> String
##% x: (Class,?String) -> Class
def x(y) 
  raise "Nope" unless y.kind_of?(Class)
end

x 2

