
def f(&b) @b = b end
def g() yield;@b.call end

c = 1
f do c + 2 end
g do c = "hi" end
