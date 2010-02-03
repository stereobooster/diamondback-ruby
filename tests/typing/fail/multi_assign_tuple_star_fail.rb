class A; end
a = A.new

x = a,a
y = a,*x

y[0].foo 
