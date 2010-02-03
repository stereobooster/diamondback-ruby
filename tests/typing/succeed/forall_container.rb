
##% B<t>
class B
  ##% set<self> ; self <= B<t> : t -> t
  def set(x) 
    @x = x
  end

  ##% get<self> ; self <= B<t> : () -> t
  def get() 
    @x
  end
end

b1 = B.new
b1.set(2)
b1.get() + 3

class C
  def z()
  end
end

b2 = B.new
b2.set(C.new)
b2.get.z

