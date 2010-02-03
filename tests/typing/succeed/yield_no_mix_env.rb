
class A
  def each() 
    yield(@contents); 
  end
end

class B
  def z() end
  def f() 
    z
    A.new.each { |x| }
  end
end
