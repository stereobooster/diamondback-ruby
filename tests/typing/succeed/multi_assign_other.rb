
class A
  def am() end
end

a = A.new

a1 = a,a

x,y,z = a,*a1
x.am
y.am
z.am
