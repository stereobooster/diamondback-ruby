
def f()
  yield(2)
end

f() {|x| x+1 }
