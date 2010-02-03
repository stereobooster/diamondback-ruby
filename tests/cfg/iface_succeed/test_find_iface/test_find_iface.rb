require 'test_find_iface/a.rb'

# This should load the .rbi file, not the .rb
# The .rb's A#foo returns a Fixnum, and will fail

a = A.new
"Hi" + a.foo
