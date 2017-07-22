
##% f<self> ; self <= Object: (String or Fixnum) -> Boolean
def f(x) 
  if x.class == String || x.class == Fixnum
    true
  else
    fail "nope"
  end
end

f(3)
f("")
