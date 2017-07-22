
class Object
  ##% foo: Fixnum -> Fixnum
  def foo(x) end
end

class A
  ##% A.foo: (String,String) -> String
  def A.foo(x,y) x.concat(y) end
end

A.foo("hi"," there")
