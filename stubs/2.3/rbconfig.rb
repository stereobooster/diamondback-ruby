
# This file was created by mkconfig.rb when ruby was built.  Any
# changes made to this file will be lost the next time ruby is built.

module Config
  RUBY_VERSION == "2.3.3" or
    raise "ruby lib version (2.3.3) doesn't match executable version (#{RUBY_VERSION})"

  TOPDIR = File.dirname(__FILE__).chomp!("/lib/ruby/2.3/x86_64-linux")
  DESTDIR = '' unless defined? DESTDIR
  CONFIG = {}
  CONFIG["DESTDIR"] = DESTDIR
  CONFIG["INSTALL"] = '/usr/bin/install -c'
  CONFIG["prefix"] = (TOPDIR || DESTDIR + "/usr")
  CONFIG["EXEEXT"] = ""
  CONFIG["ruby_install_name"] = "ruby2.3"
  CONFIG["RUBY_INSTALL_NAME"] = "ruby2.3"
  CONFIG["RUBY_SO_NAME"] = "ruby2.3"
  CONFIG["SHELL"] = "/bin/sh"
  CONFIG["PATH_SEPARATOR"] = ":"
  CONFIG["PACKAGE_NAME"] = ""
  CONFIG["PACKAGE_TARNAME"] = ""
  CONFIG["PACKAGE_VERSION"] = ""
  CONFIG["PACKAGE_STRING"] = ""
  CONFIG["PACKAGE_BUGREPORT"] = ""
  CONFIG["exec_prefix"] = "$(prefix)"
  CONFIG["bindir"] = "$(exec_prefix)/bin"
  CONFIG["sbindir"] = "$(exec_prefix)/sbin"
  CONFIG["libexecdir"] = "$(prefix)/lib/ruby2.3"
  CONFIG["datarootdir"] = "$(prefix)/share"
  CONFIG["datadir"] = "$(datarootdir)"
  CONFIG["sysconfdir"] = "$(DESTDIR)/etc"
  CONFIG["sharedstatedir"] = "$(prefix)/com"
  CONFIG["localstatedir"] = "$(DESTDIR)/var"
  CONFIG["includedir"] = "$(prefix)/include"
  CONFIG["oldincludedir"] = "/usr/include"
  CONFIG["docdir"] = "$(datarootdir)/doc/$(PACKAGE)"
  CONFIG["infodir"] = "$(prefix)/share/info"
  CONFIG["htmldir"] = "$(docdir)"
  CONFIG["dvidir"] = "$(docdir)"
  CONFIG["pdfdir"] = "$(docdir)"
  CONFIG["psdir"] = "$(docdir)"
  CONFIG["libdir"] = "$(exec_prefix)/lib"
  CONFIG["localedir"] = "$(datarootdir)/locale"
  CONFIG["mandir"] = "$(prefix)/share/man"
  CONFIG["ECHO_C"] = ""
  CONFIG["ECHO_N"] = "-n"
  CONFIG["ECHO_T"] = ""
  CONFIG["LIBS"] = "-lpthread -ldl -lcrypt -lm "
  CONFIG["build_alias"] = "x86_64-linux-gnu"
  CONFIG["host_alias"] = ""
  CONFIG["target_alias"] = ""
  CONFIG["MAJOR"] = "1"
  CONFIG["MINOR"] = "8"
  CONFIG["TEENY"] = "6"
  CONFIG["build"] = "x86_64-pc-linux-gnu"
  CONFIG["build_cpu"] = "x86_64"
  CONFIG["build_vendor"] = "pc"
  CONFIG["build_os"] = "linux-gnu"
  CONFIG["host"] = "x86_64-pc-linux-gnu"
  CONFIG["host_cpu"] = "x86_64"
  CONFIG["host_vendor"] = "pc"
  CONFIG["host_os"] = "linux-gnu"
  CONFIG["target"] = "x86_64-pc-linux-gnu"
  CONFIG["target_cpu"] = "x86_64"
  CONFIG["target_vendor"] = "pc"
  CONFIG["target_os"] = "linux"
  CONFIG["CC"] = "cc"
  CONFIG["CFLAGS"] = "-fno-strict-aliasing -g -O2  -fPIC"
  CONFIG["LDFLAGS"] = "-L.  -rdynamic -Wl,-export-dynamic"
  CONFIG["CPPFLAGS"] = ""
  CONFIG["OBJEXT"] = "o"
  CONFIG["CPP"] = "cc -E"
  CONFIG["GREP"] = "/bin/grep"
  CONFIG["EGREP"] = "/bin/grep -E"
  CONFIG["GNU_LD"] = "yes"
  CONFIG["CPPOUTFILE"] = "-o conftest.i"
  CONFIG["OUTFLAG"] = "-o "
  CONFIG["YACC"] = "bison -y"
  CONFIG["YFLAGS"] = ""
  CONFIG["RANLIB"] = "ranlib"
  CONFIG["AR"] = "ar"
  CONFIG["AS"] = "as"
  CONFIG["ASFLAGS"] = ""
  CONFIG["NM"] = ""
  CONFIG["WINDRES"] = ""
  CONFIG["DLLWRAP"] = ""
  CONFIG["OBJDUMP"] = ""
  CONFIG["LN_S"] = "ln -s"
  CONFIG["SET_MAKE"] = ""
  CONFIG["INSTALL_PROGRAM"] = "$(INSTALL)"
  CONFIG["INSTALL_SCRIPT"] = "$(INSTALL)"
  CONFIG["INSTALL_DATA"] = "$(INSTALL) -m 644"
  CONFIG["RM"] = "rm -f"
  CONFIG["CP"] = "cp"
  CONFIG["MAKEDIRS"] = "mkdir -p"
  CONFIG["ALLOCA"] = ""
  CONFIG["DLDFLAGS"] = ""
  CONFIG["ARCH_FLAG"] = ""
  CONFIG["STATIC"] = ""
  CONFIG["CCDLFLAGS"] = " -fPIC"
  CONFIG["LDSHARED"] = "$(CC) -shared"
  CONFIG["DLEXT"] = "so"
  CONFIG["DLEXT2"] = ""
  CONFIG["LIBEXT"] = "a"
  CONFIG["LINK_SO"] = ""
  CONFIG["LIBPATHFLAG"] = " -L\"%s\""
  CONFIG["RPATHFLAG"] = ""
  CONFIG["LIBPATHENV"] = "LD_LIBRARY_PATH"
  CONFIG["TRY_LINK"] = ""
  CONFIG["STRIP"] = "strip -S -x"
  CONFIG["EXTSTATIC"] = ""
  CONFIG["setup"] = "Setup"
  CONFIG["MINIRUBY"] = "./miniruby$(EXEEXT)"
  CONFIG["PREP"] = "miniruby$(EXEEXT)"
  CONFIG["RUNRUBY"] = "$(MINIRUBY) $(srcdir)/runruby.rb --extout=$(EXTOUT) --"
  CONFIG["EXTOUT"] = ".ext"
  CONFIG["ARCHFILE"] = ""
  CONFIG["RDOCTARGET"] = ""
  CONFIG["XCFLAGS"] = " -DRUBY_EXPORT -D_GNU_SOURCE=1"
  CONFIG["XLDFLAGS"] = ""
  CONFIG["LIBRUBY_LDSHARED"] = "$(CC) -shared"
  CONFIG["LIBRUBY_DLDFLAGS"] = "-Wl,-soname,lib$(RUBY_SO_NAME).so.$(MAJOR).$(MINOR)"
  CONFIG["rubyw_install_name"] = ""
  CONFIG["RUBYW_INSTALL_NAME"] = ""
  CONFIG["LIBRUBY_A"] = "lib$(RUBY_SO_NAME)-static.a"
  CONFIG["LIBRUBY_SO"] = "lib$(RUBY_SO_NAME).so.$(MAJOR).$(MINOR).$(TEENY)"
  CONFIG["LIBRUBY_ALIASES"] = "lib$(RUBY_SO_NAME).so.$(MAJOR).$(MINOR) lib$(RUBY_SO_NAME).so"
  CONFIG["LIBRUBY"] = "$(LIBRUBY_SO)"
  CONFIG["LIBRUBYARG"] = "$(LIBRUBYARG_SHARED)"
  CONFIG["LIBRUBYARG_STATIC"] = "-l$(RUBY_SO_NAME)-static"
  CONFIG["LIBRUBYARG_SHARED"] = "-l$(RUBY_SO_NAME)"
  CONFIG["SOLIBS"] = "$(LIBS)"
  CONFIG["DLDLIBS"] = " -lc"
  CONFIG["ENABLE_SHARED"] = "yes"
  CONFIG["MAINLIBS"] = ""
  CONFIG["COMMON_LIBS"] = ""
  CONFIG["COMMON_MACROS"] = ""
  CONFIG["COMMON_HEADERS"] = ""
  CONFIG["EXPORT_PREFIX"] = ""
  CONFIG["MAKEFILES"] = "Makefile"
  CONFIG["arch"] = "x86_64-linux"
  CONFIG["sitearch"] = "x86_64-linux"
  CONFIG["sitedir"] = "$(DESTDIR)/usr/local/lib/site_ruby"
  CONFIG["configure_args"] = " '--build=x86_64-linux-gnu' '--prefix=/usr' '--includedir=$(prefix)/include' '--mandir=$(prefix)/share/man' '--infodir=$(prefix)/share/info' '--sysconfdir=/etc' '--localstatedir=/var' '--libexecdir=$(prefix)/lib/ruby2.3' '--disable-maintainer-mode' '--disable-dependency-tracking' '--srcdir=.' '--program-suffix=2.3' '--enable-pthread' '--enable-shared' '--enable-ipv6' '--with-lookup-order-hack=INET' '--disable-rpath' '--disable-install-doc' '--with-sitedir=/usr/local/lib/site_ruby' '--with-default-kcode=none' '--with-dbm-type=gdbm_compat' '--with-tklib=tk8.4' '--with-tcllib=tcl8.4' '--with-tcl-include=/usr/include/tcl8.4' '--with-bundled-sha1' '--with-bundled-md5' '--with-bundled-rmd160' 'build_alias=x86_64-linux-gnu' 'CC=cc' 'CFLAGS=-fno-strict-aliasing -g -O2' 'LDFLAGS=' 'CPPFLAGS='"
  CONFIG["NROFF"] = "/bin/false"
  CONFIG["MANTYPE"] = "man"
  CONFIG["ruby_version"] = "$(MAJOR).$(MINOR)"
  CONFIG["rubylibdir"] = "$(libdir)/ruby/$(ruby_version)"
  CONFIG["archdir"] = "$(rubylibdir)/$(arch)"
  CONFIG["sitelibdir"] = "$(sitedir)/$(ruby_version)"
  CONFIG["sitearchdir"] = "$(sitelibdir)/$(sitearch)"
  CONFIG["topdir"] = File.dirname(__FILE__)
  MAKEFILE_CONFIG = {}
  CONFIG.each{|k,v| MAKEFILE_CONFIG[k] = v.dup}
  def Config::expand(val, config = CONFIG)
    val.gsub!(/\$\$|\$\(([^()]+)\)|\$\{([^{}]+)\}/) do |var|
      if !(v = $1 || $2)
	'$'
      elsif key = config[v = v[/\A[^:]+(?=(?::(.*?)=(.*))?\z)/]]
	pat, sub = $1, $2
	config[v] = false
	Config::expand(key, config)
	config[v] = key
	key = key.gsub(/#{Regexp.quote(pat)}(?=\s|\z)/n) {sub} if pat
	key
      else
	var
      end
    end
    val
  end
  CONFIG.each_value do |val|
    Config::expand(val)
  end
end
RbConfig = Config # compatibility for ruby-1.9
CROSS_COMPILING = nil unless defined? CROSS_COMPILING
