
def f(&b) @b = b end
def g() yield;@b.call end

f do c = 1; c + 2 end
g do c = "hi" end
