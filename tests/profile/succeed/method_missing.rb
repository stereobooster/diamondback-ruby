
class A
  def method_missing(name,*args)
    f(*args)
  end

  def f(*args) args.empty? end
end

a = A.new

a.asdf(1)
a.basd(2,3)
