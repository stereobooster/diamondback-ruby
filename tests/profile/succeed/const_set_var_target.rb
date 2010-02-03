
__END__
class A
  def set(x)
    self.class.const_set(:X,x)
  end
end

class B < A
end

a = A.new
b = B.new
a.set(3)
b.set("hi")
A::X - 5
B::X.concat "blah"

