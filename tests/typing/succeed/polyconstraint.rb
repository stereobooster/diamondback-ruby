
##% A<t>; t <= Fixnum
class A

  ##% set<self> ; self <= A<t>: t -> t
  def set(x) @x=x end

  ##% get<self> ; self <= A<t>: () -> t
  def get() @x end
end

a = A.new
a.set 2
a.get + 3
