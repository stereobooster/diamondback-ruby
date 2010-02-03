
class A
  class_eval do
    def f(x) x end
  end
end

A.new.f(2) + 3
