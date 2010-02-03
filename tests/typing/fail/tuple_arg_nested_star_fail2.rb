
def f() yield [1,2,true] end
f {|(x,*y)| y[1] + 2}
