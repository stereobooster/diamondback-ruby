
class A
  def f() end
end

##% f : () -> Fixnum
##% f : (Fixnum, *Fixnum) -> A
def f(*y)
  return 3 if y.empty?
  for i in y do
    2 + i
  end
  A.new
end

f() + 3
