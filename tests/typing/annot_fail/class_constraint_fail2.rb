
##% A<t> ; t <= Fixnum
class A
  ##% get<self> ; self <= A<t> : () -> t 
  def get() @x end

  ##% set<self> ; self <= A<t> : t -> t
  def set(x) @x = x end

end

class B < A
  def get() "err" end
end

a = B.new
a.set(3)
a.get + 4
