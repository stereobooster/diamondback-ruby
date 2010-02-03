

class A
  def A.f() @@x end
  def A.g() class_variable_set("@@x",3) end
end

A.g
A.f + 3
