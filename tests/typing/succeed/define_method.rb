
class A
  define_method :f do |x| x+2 end
end

a = A.new
a.f(1) + 3
