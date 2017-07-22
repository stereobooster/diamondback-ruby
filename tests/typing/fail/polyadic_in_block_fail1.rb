
g = proc do |x|
  x + 2
end

h = proc do |x,y|
  x.concat("hi")
end

g.call(3,4)
h.call("t",4)
