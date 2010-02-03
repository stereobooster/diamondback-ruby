def f()

  a = proc {
    return 3
  }
  a.call
  "a"
end

f.concat "b"
