
class A

  FIELDS = %w(x y)
  FIELDS.each {|f|
    attr_reader f
  }

  def initialize()
    @x = 1
    @y = "hi"
  end
end

a = A.new

if false
  a.x.concat " world"
end

