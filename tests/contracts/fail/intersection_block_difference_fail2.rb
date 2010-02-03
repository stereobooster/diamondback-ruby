
class A
  ##% str: () {String -> Fixnum} -> Fixnum
  ##% str: () {String -> String} -> String
  def str() 
    yield("hi","bye")
  end
end

a = A.new
a.str {|x| x.concat " world"}
