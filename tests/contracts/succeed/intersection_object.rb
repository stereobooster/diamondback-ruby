
class A
  def foo() "hi" end
end

class B
  def bar() "hi" end
end

class C
  ##% choose: [foo: () -> String] -> String
  ##% choose: [bar: () -> String] -> String
  def choose(x) 
    return "blah"
    if x.respond_to? :foo
      x.foo
    else 
      x.bar
    end
  end
end

c = C.new
c.choose A.new
c.choose B.new
