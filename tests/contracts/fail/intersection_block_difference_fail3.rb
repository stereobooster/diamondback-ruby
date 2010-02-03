
class A
  ##% str: () {String -> Fixnum} -> Fixnum
  ##% str: () {String -> String} -> String
  def str() 
    yield("hi")
    3
  end
end

a = A.new
a.str {|x| x.concat " world"}
