
def f(x,y,&blk)
  x + y
end


def g(&blk)
  f(1,2,&blk)
end

g()
