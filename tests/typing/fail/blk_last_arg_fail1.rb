
def f(x,y,&blk)
  x + y
end


def g(&blk)
  f(1,&blk)
end


g()
