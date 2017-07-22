
class Object
  ##% foo: Fixnum -> Fixnum
  def foo() end
end

class A
  ##% A.foo: (String,String) -> String
  def A.foo(x,y) x.concat(y) end
end

A.foo(2)
