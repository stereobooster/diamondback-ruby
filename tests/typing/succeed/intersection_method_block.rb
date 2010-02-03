
##% f: (String,String) -> String
##% f: (String,String) {String -> String} -> Fixnum
def f(x,y) 
  if block_given? then 3 else "blah" end
end

w = f("a","b") {|z| z}
w + 2
