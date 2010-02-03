
##% A<t> ; t <= Fixnum
class A
  ##% get<self> ; self <= A<t> : () -> t 
  def get() @x end

  ##% set<self> ; self <= A<t> : t -> t
  def set(x) @x = x end

end

a = A.new
a.set(4)
a.get + 5
