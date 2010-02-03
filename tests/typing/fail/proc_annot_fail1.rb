

##% f: () -> Proc<^(Fixnum,Fixnum),Fixnum>
def f(); Proc.new {|x,y| x+y} end

f.call(2)+4
