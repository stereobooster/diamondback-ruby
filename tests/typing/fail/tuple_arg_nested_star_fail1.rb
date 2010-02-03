
def f() yield [1,2] end
f {|(x,*y)| y + 2}
