
module M
  def f()
  end
end


class A
end

A.extend(M)
A.new.f


