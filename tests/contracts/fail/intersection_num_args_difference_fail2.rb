
class A
  
  ##% foo : (String, String) -> String
  ##% foo : (String, Fixnum, String) -> Fixnum
  def foo(x1,x2)
    x2
  end
end

a = A.new

a.foo("a","b").concat "c"
a.foo("a",3,"b") - 2
