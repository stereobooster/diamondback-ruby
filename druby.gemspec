Gem::Specification.new do |s|
  s.name = "diamondback-ruby"
  s.authors = ["Michael Furr", "David An"]
  s.homepage = "http://www.cs.umd.edu/projects/PL/druby/"
  s.email = "druby-discuss@mailman.cs.umd.edu"
  s.license = "GPL-2.0"
  s.summary = "Static type inference system for Ruby"
  s.description = "Dimondback Ruby is a static type inference system for Ruby"
  s.version = File.open("OMakefile") {|f| f.find {|l| l=~/VERSION=(.*)/}} && $1
  s.executables = ["druby","safe_eval"]
  s.extensions = ["configure"]
  s.bindir = "gem_bin"
  s.files = [
    "gem_bin/druby", "gem_bin/druby.real",
     "gem_bin/safe_eval", "gem_bin/safe_eval.real",
     "druby.conf.in", "configure",
     "lib/*.rb",
     "lib/druby/*.rb","lib/druby/**/*.rb",
     "stubs/2.3/*.rb","stubs/2.3/**/*.rb",
     "LICENSE", "AUTHORS"
  ].map { |f| File.expand_path("../#{f}", __FILE__) }
  .map { |f| Dir.glob(f) }.flatten
  s.platform = Gem::Platform::CURRENT
end
