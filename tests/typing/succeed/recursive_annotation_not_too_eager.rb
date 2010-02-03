
##% Blah<t>
class Blah
  ##% before : () -> Blah<t>
  def before() end
  ## In our substitution semantcs, we can't create this type yet,
  ## since it won't contain [after] 
  ##% foo <u> : u -> Blah<t or u>
  def foo(p0) end
  def after() end
end

class A
end

class B
  
  def initialize
    @blah = Blah.new
  end

  ##% add: A -> Blah<A>
  def add(a)
    @blah.foo a
  end

end

a = A.new
b = B.new 
b.add a
