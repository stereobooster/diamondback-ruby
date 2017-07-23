# for some reason this is not detectd
# {}[:a]

# as of now it supports only old syntax of with hash-rocket supported
a = {:b => 1}
a[:c] || raise("key not found: :c")

# for some reason this does not work
# a.fatch(:c)
