
module M; end

class A
  include M
end
a = A.new
M.module_eval "def f(x) x end"
if false
  a.f() + 3
end

