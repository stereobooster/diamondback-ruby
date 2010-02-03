
class A
  class_eval do
    def f(x) x end
  end
end

if false
  A.new.f()
end
