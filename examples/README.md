# Ruby static type checker - examples

[![Hacker News](https://img.shields.io/badge/Hacker%20News-Y-orange.svg)](https://news.ycombinator.com/item?id=15526406)

This is proof of concept of ruby a static typechecker. It is similar to what [Facebook Flow](https://github.com/facebook/flow) tries to do for JavaScript. This page shows some typical errors in Ruby which can be checked statically instead of guarding against them at runtime. Error messages look cryptic - this can be changed but needs more work.

## 1

```ruby
"abc" + 1
```

### DRuby

```
gem_bin/druby examples/test1.rb
[ERROR] instance Fixnum does not support methods to_str
  in creating instance of Fixnum
  at ./examples/test1.rb:1
  in typing expression 1
  at ./examples/test1.rb:1
  in typing actual argument 1
  at ./examples/test1.rb:1
  in method call %{abc}.+
  at ./examples/test1.rb:1
```

### Ruby

```
ruby examples/test1.rb
examples/test1.rb:1:in `+': no implicit conversion of Fixnum into String (TypeError)
	from examples/test1.rb:1:in `<main>'
```

## 2

```ruby
"abc".gsub("a", "A", 1)
```

### DRuby

```
gem_bin/druby examples/test2.rb
[ERROR] subtype relation failed for all members of intersection type
  in intersection with rhs:
  in solving method: gsub
  in closed solving instance String <= ?:[gsub,]
  in method call %{abc}.gsub
  at ./examples/test2.rb:1
```

### Ruby

```
examples/test2.rb:1:in `gsub': wrong number of arguments (given 3, expected 1..2) (ArgumentError)
	from examples/test2.rb:1:in `<main>'
```

## 3

```ruby
[1,2,3][:a]
```

### DRuby

```
gem_bin/druby examples/test3.rb
[ERROR] subtype relation failed for all members of intersection type
  in intersection with rhs:
  in solving method: []
```

### Ruby

```
ruby examples/test3.rb
examples/test3.rb:1:in `[]': no implicit conversion of Symbol into Integer (TypeError)
	from examples/test3.rb:1:in `<main>'
```

### Actual

Expected Object, Array given.

## 4

```ruby
class A; end
A.new.echo
```

### DRuby

```
gem_bin/druby examples/test4.rb
[ERROR] instance A does not support methods echo
  in method call echo
  at ./examples/test4.rb:5
  in typing ::A.new
  at ./examples/test4.rb:5
```

### Ruby

```
ruby examples/test4.rb
examples/test4.rb:5:in `<main>': undefined method `echo' for #<A:0x007f977208dc50> (NoMethodError)
```

## 5

```ruby
a = {:b => 1}
a[:c] || raise("key not found: :c")
```

### DRuby

```
gem_bin/druby examples/test5.rb
DRuby analysis complete.
[ERROR] This record contains no field named :"c"
```

### Ruby

```
ruby examples/test5.rb
examples/test5.rb:6:in `<main>': key not found: :c (RuntimeError)
```

## 6

```ruby
B.new
```

### DRuby

```
gem_bin/druby examples/test6.rb
[ERROR] Unable to statically locate scope B in namespace hierarchy at ::
  at ./examples/test6.rb:1
```

### Ruby

```
ruby examples/test6.rb
examples/test6.rb:1:in `<main>': uninitialized constant B (NameError)
```
