
class A
  def foo() 3 end
end

class C
  ##% choose: [foo: () -> String] -> []
  ##% choose: [bar: () -> String] -> []
  def choose(x) end
end

c = C.new
c.choose A.new
