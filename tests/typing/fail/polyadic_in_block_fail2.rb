
g = proc do |x|
  x + 2
end

h = proc do |x,y|
  x.concat("hi")
end

g.call("blah")
h.call(3,4)
