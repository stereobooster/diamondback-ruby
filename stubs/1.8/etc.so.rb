class Struct
  class Passwd < Struct
    include Enumerable
    ##% dir : () -> String
    def dir(*) end
    ##% dir= : String -> String
    def dir=(p0) end
    ##% gecos : () -> String
    def gecos(*) end
    ##% gecos= : String -> String
    def gecos=(p0) end
    ##% gid : () -> Fixnum
    def gid(*) end
    ##% gid= : Fixnum -> Fixnum
    def gid=(p0) end
    ##% name : () -> String
    def name(*) end
    ##% name= : String -> String
    def name=(p0) end
    ##% passwd : () -> String
    def passwd(*) end
    ##% passwd= : String -> String
    def passwd=(p0) end
    ##% shell : () -> String
    def shell(*) end
    ##% shell= : String -> String
    def shell=(p0) end
    ##% uid : () -> Fixnum
    def uid(*) end
    ##% uid= : Fixnum -> Fixnum
    def uid=(p0) end
    ##% Passwd."[]" : String -> Passwd
    def Passwd.[](*) end
    ##% Passwd.members : () -> Array<String>
    def Passwd.members(*) end
    ##% initialize : (?String,?String,?Fixnum,?Fixnum,?String,?String,?String) -> Passwd
    def initialize(*) end
  end

  class Group < Struct
    include Enumerable
    ##% gid : () -> Fixnum
    def gid(*) end
    ##% gid= : Fixnum -> Fixnum
    def gid=(p0) end
    ##% mem : () -> Array<String>
    def mem(*) end
    ##% mem= : Array<String> -> Array<String>
    def mem=(p0) end
    ##% name : () -> String
    def name(*) end
    ##% name= : String -> String
    def name=(p0) end
    ##% passwd : () -> String
    def passwd(*) end
    ##% passwd= : String -> String
    def passwd=(p0) end
    ##% Group."[]" : String -> Group
    def Group.[](*rest) end
    ##% Group.members : () -> Array<String>
    def Group.members(*) end
    ##% initialize : (?String,?String,?Fixnum,?Array<String>) -> Group
    def initialize(*) end
  end
end

module Etc
  ##% Etc.endgrent : !FIXME -> !FIXME
  def Etc.endgrent(*) end
  ##% Etc.endpwent : !FIXME -> !FIXME
  def Etc.endpwent(*) end
  ##% Etc.getgrent : !FIXME -> !FIXME
  def Etc.getgrent(*) end
  ##% Etc.getgrgid : Fixnum -> Struct::Group
  def Etc.getgrgid(p0) end
  ##% Etc.getgrnam : String -> Struct::Group
  def Etc.getgrnam(p0) end
  ##% Etc.getlogin : !FIXME -> !FIXME
  def Etc.getlogin(*) end
  ##% Etc.getpwent : !FIXME -> !FIXME
  def Etc.getpwent(*) end
  ##% Etc.getpwnam : String -> Struct::Passwd
  def Etc.getpwnam(p0) end
  ##% Etc.getpwuid : (?Fixnum) -> Struct::Passwd
  def Etc.getpwuid(*rest) end
  ##% Etc.group : !FIXME -> !FIXME
  def Etc.group(*) end
  ##% Etc.passwd : !FIXME -> !FIXME
  def Etc.passwd(*) end
  ##% Etc.setgrent : !FIXME -> !FIXME
  def Etc.setgrent(*) end
  ##% Etc.setpwent : !FIXME -> !FIXME
  def Etc.setpwent(*) end
end

