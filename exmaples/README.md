# Examples

Many typical errors are caught by static type checker, though error messages are cryptic. Note: ruby error messages sometimes cryptic too.

## 1

```ruby
"abc" + 1
```

### DRuby

```
gem_bin/druby exmaples/test1.rb
[ERROR] instance Fixnum does not support methods to_str
  in creating instance of Fixnum
  at ./exmaples/test1.rb:1
  in typing expression 1
  at ./exmaples/test1.rb:1
  in typing actual argument 1
  at ./exmaples/test1.rb:1
  in method call %{abc}.+
  at ./exmaples/test1.rb:1
```

### Ruby

```
ruby exmaples/test1.rb
exmaples/test1.rb:1:in `+': no implicit conversion of Fixnum into String (TypeError)
	from exmaples/test1.rb:1:in `<main>'
```

## 2

```ruby
"abc".gsub("a", "A", 1)
```

### DRuby

```
gem_bin/druby exmaples/test2.rb
[ERROR] subtype relation failed for all members of intersection type
  in intersection with rhs:
  in solving method: gsub
  in closed solving instance String <= ?:[gsub,]
  in method call %{abc}.gsub
  at ./exmaples/test2.rb:1
```

### Ruby

```
exmaples/test2.rb:1:in `gsub': wrong number of arguments (given 3, expected 1..2) (ArgumentError)
	from exmaples/test2.rb:1:in `<main>'
```

## 3

```ruby
[1,2,3][:a]
```

### DRuby

```
gem_bin/druby exmaples/test3.rb
[ERROR] subtype relation failed for all members of intersection type
  in intersection with rhs:
  in solving method: []
```

### Ruby

```
ruby exmaples/test3.rb
exmaples/test3.rb:1:in `[]': no implicit conversion of Symbol into Integer (TypeError)
	from exmaples/test3.rb:1:in `<main>'
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
gem_bin/druby exmaples/test4.rb
[ERROR] instance A does not support methods echo
  in method call echo
  at ./exmaples/test4.rb:5
  in typing ::A.new
  at ./exmaples/test4.rb:5
```

### Ruby

```
ruby exmaples/test4.rb
exmaples/test4.rb:5:in `<main>': undefined method `echo' for #<A:0x007f977208dc50> (NoMethodError)
```

## 5

```ruby
a = {:b => 1}
a[:c] || raise("key not found: :c")
```

### DRuby

```
gem_bin/druby exmaples/test5.rb
DRuby analysis complete.
[ERROR] This record contains no field named :"c"
```

### Ruby

```
ruby exmaples/test5.rb
exmaples/test5.rb:6:in `<main>': key not found: :c (RuntimeError)
```
