
module M; end

class A
  include M
end
a = A.new
M.module_eval "def f(x) x end"
a.f(2) + 3

