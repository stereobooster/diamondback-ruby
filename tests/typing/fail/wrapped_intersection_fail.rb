
##% f: (String,String) -> String
##% f: (Fixnum,String) -> Fixnum
def f(x,y) 
  if Fixnum == x.class then 0 else y end
end

def g(z)
  f(z,"hi") + 2
end

g("bye")
