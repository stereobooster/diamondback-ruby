
module M
  def f()
  end
end


class A
end

a = A.new
a.extend(M)
A.f


