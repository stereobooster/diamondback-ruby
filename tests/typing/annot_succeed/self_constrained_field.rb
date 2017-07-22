
class A

  ##% f<self> ; self <= [@v : String] : () -> Fixnum 
  def f()
    3
  end

  def g()
    @v = "hi"
  end
  
end
 
a = A.new
a.g
a.f
