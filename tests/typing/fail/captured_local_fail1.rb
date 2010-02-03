
class A
  def g(&b)
    @blk = b
  end
  def callit() @blk.call end
end

x = "hi"
a = A.new
a.g { x = 2 }
x = "bye"
a.callit
x.concat " there"

