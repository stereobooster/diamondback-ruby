class ID
  def id(x)
    x
  end
end

ID.new.id(3) + 3
ID.new.id("foo") + "bar"
