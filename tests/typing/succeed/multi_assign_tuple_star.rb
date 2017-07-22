
class A
  def am() end
end

a = A.new

x = a,a
y = a,*x

y[0].am 
