
##% f : () -> String
##% f<t> : () {Fixnum -> t} -> t
def f()
  if block_given? then yield(3) else "blah" end
end

z = f() {|x| 3}

z + 2
