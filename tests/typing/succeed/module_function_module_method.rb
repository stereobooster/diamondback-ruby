
module M
  def foo() 3 end
  module_function :foo
end

M::foo + 4
