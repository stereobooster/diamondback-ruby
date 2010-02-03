
class A
  def a() end
end

class B
  def b() end
end

##% foo : A -> A
##% foo : B -> B
def foo(x) 
  3
end

foo(A.new)
foo(B.new)
