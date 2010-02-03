#!/usr/bin/ruby

if ARGV.size < 1 then 
  print "#{$0} <require_file>\n"
  exit(1)
end

orig_consts = Object.constants
require ARGV[0]
new_consts = Object.constants - orig_consts

exec("ruby -r #{ARGV[0]} ./extract_types.rb #{new_consts.join(" ")}")
