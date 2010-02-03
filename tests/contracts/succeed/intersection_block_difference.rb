
class A
  ##% str: () {String -> Fixnum} -> Fixnum
  ##% str: () {String -> String} -> String
  def str() 
    yield("hi")
  end
end

a = A.new
a.str {|x| x.concat " world"}
