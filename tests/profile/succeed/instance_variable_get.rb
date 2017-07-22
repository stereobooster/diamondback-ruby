
class A
  def initialize() @x = 1 end
end

a = A.new
x = a.instance_variable_get("@x")
x + 3

#         intercept_args Kernel, :instance_variable_get, &store #Object?
#         intercept_args Kernel, :instance_variable_set, &store #Object?
#         intercept_args Module, :class_variable_get, &store
#         intercept_args Module, :class_variable_set, &store
#         intercept_args Module, :const_get, &store
#         intercept_args Module, :const_set, &store
