
require 'rake'
Gem::Specification.new do |s|

  s.name = "diamondback-ruby"
  s.authors = ["Michael Furr", "David An"]
  s.homepage = "http://www.cs.umd.edu/projects/PL/druby/"
  s.email = "druby-discuss@mailman.cs.umd.edu"
  s.summary = "Static type inference system for Ruby"
  s.description = <<EOM
Dimondback Ruby is a static type inference system for Ruby
EOM

  s.version = File.open("OMakefile") {|f| f.find {|l| l=~/VERSION=(.*)/}} && $1

  s.executables = ["druby","safe_eval"]
  s.extensions = ["configure"]
  s.bindir = "gem_bin"
  s.files = FileList["gem_bin/druby", "gem_bin/druby.real", 
                     "gem_bin/safe_eval", "gem_bin/safe_eval.real",
                     "druby.conf.in", "configure",
                     "lib/*.rb",
                     "lib/druby/*.rb","lib/druby/**/*.rb",
                     "stubs/1.8/*.rb","stubs/1.8/**/*.rb",
                     "LICENSE", "AUTHORS"
                    ]
  s.platform = Gem::Platform::CURRENT

end

#Local Variables: 
# mode:ruby
#End:
