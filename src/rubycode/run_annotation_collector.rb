#!/opt/local/bin/ruby

# == Description
#
# wrapper script to run type profiler against rspec
#
# == Usage
#
# Required parameters:
#   --spec        path to rubyspec top level directory
#
#   --mspec_bin   path to mspec bin directory (mspec/bin)
#
#   --output      path to directory in which to store generated int
#
# Optional Parameters:
#   --type_prof   path to type prof module to use for this profiling
#                 execution (default=./type_prof.rb)
#
#   --force       overwrite existing annotation files in output directory
#
#   --quit_on_err stop running subsequent rspec tests if any child process
#                 returns a non-zero error code (good for debugging, generates
#                 incomplete annotations for standard library)
#
#   --quiet       suppress printing of executed rspec commands
#

#========================================

require 'getoptlong'
require 'rdoc/usage'

params =
  [
   ['--help', '-h', GetoptLong::NO_ARGUMENT],
   ['--spec', '-s', GetoptLong::REQUIRED_ARGUMENT],
   ['--mspec_bin', '-m', GetoptLong::REQUIRED_ARGUMENT],
   ['--output', '-o', GetoptLong::REQUIRED_ARGUMENT],
   ['--type_prof', '-t', GetoptLong::OPTIONAL_ARGUMENT],
   ['--force', '-f', GetoptLong::NO_ARGUMENT],
   ['--quit_on_err', '-e', GetoptLong::NO_ARGUMENT],
   ['--quiet', '-q', GetoptLong::NO_ARGUMENT],
  ]

opts = GetoptLong.new(*params)

spec_path = nil
mspec_bin_path = nil
output_dir = nil
type_prof_path = "./type_prof.rb"
force = false
quit_on_err = false
quiet = false

opts.each {|param, arg|
  case param
  when '--help'
    RDoc::usage
  when '--spec'
    spec_path = arg
  when '--mspec_bin'
    mspec_bin_path = arg
  when '--output'
    output_dir = arg
  when '--type_prof'
    type_prof_path = arg
  when '--force'
    force = true
  when '--quit_on_err'
    quit_on_err = true
  when '--quiet'
    quiet = true
  end
}

required_params = {
  'spec' => spec_path,
  'mspec_bin' => mspec_bin_path,
  'output' => output_dir}
missing_options = required_params.select {|k, v| v == nil}
if(missing_options.length > 0)
  puts "missing option%s: %s" % [missing_options.length > 1 ? "s" : "",
                                 missing_options.map {|x| x[0]}.join(", ")]
  RDoc::usage
end

unless(File.directory?(spec_path))
  puts "Path to rubyspec directory '#{spec_path}' " +
    "is not a directory: #{$!}"
  RDoc::usage
end

unless(File.directory?(mspec_bin_path))
  puts "Path to mspec bin directory '#{mspec_bin_path}' " +
    "is not a directory: #{$!}"
  RDoc::usage
end

unless(File.file?(type_prof_path))
  puts "Specified type profiling module '#{type_prof_path}' " +
    "does not exist: #{$!}"
  RDoc::usage
end

unless(File.directory?(output_dir))
  puts "Output directory '#{output_dir}' does not exist -- creating"
  Dir.mkdir(output_dir)
end

#========================================

spec_groups = %w[core library]

mspec_run = File.join(mspec_bin_path, "mspec-run")

for group in spec_groups
  specs = Dir.entries(File.join(spec_path, group)).reject {|x| x =~ /^\.\.?$/}

  unless(quiet)
    puts "--------------------"
    puts "found #{specs.length} specs in group #{group}"
    puts "--------------------"
  end

  for job in specs
    output_file = File.join(output_dir, "#{group}.#{job}.annotations.txt")
    if(File.exists?(output_file))
      if(!quiet || !force)
        puts "WARNING:  output file at #{output_file} already exists"
      end
      unless(force)
        puts "'--force' option not specified when output file exists -- exiting"
        exit 1
      end
    end

    job_path = File.join(spec_path, group, job)
    this_cmd = "ruby -r #{type_prof_path} #{mspec_run} " +
      "#{job_path} > #{output_file}"

    puts "executing '#{this_cmd}'" unless(quiet)
    if(quit_on_err)
      if(not system(this_cmd))
        puts "Child process for #{group}::#{job} exited with code #{$?} -- " +
          "terminating annotation generation run"
        exit 1
      end
    else
      system(this_cmd)
    end
  end
end

unless(quiet)
  puts "--------------------"
  puts "done generating annotations"
end

#========================================
