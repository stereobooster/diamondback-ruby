
class A
  def foo() "blah" end
end

class C
  ##% choose: [foo: () -> String] -> []
  ##% choose: [bar: () -> String] -> []
  def choose(x) end
end

c = C.new
c.choose A.new
