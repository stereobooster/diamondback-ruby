##% A<t>
class A

  ##% f<self> ; self <= [@x : t]: t -> Boolean
  def f(x)
    @x = x
    true
  end

  ##% get<self> ; self <= [@x : t] : () -> t 
  def get() @x end
  
  def g(x)
    @x = x
  end
end

a = A.new

a.f(3)
a.get + 4
a.g(7)
a.get - 4
