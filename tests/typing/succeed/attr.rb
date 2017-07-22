
class A
  attr :ro
  attr(:rw,true)
end


a = A.new
a.ro
a.rw = 4
a.rw
