
class Object
  def foo(x) 
    1
  end
end

class A
  def A.foo() 
    "blah"
  end
end

module M
end

class B < A
  include M
end

B.foo(2)
