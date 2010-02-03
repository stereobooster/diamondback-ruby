
def m(b)
  x = 1
  if(b)
    x + 1
  else
    raise "early"
    x.foo
  end
end

def test_succeed
  m(true)
end
def test_failure
  m(false)
end
