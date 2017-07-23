

module YAML
  module YAML::Syck
    GenericResolver = ::YAML::Syck::Resolver.new
    class DefaultKey
    end

    class Node
      ##% emitter : !FIXME -> !FIXME
      def emitter() end
      ##% emitter= : !FIXME -> !FIXME
      def emitter=(p0) end
      ##% kind : !FIXME -> !FIXME
      def kind() end
      ##% resolver : !FIXME -> !FIXME
      def resolver() end
      ##% resolver= : !FIXME -> !FIXME
      def resolver=(p0) end
      ##% transform : !FIXME -> !FIXME
      def transform() end
      ##% type_id : !FIXME -> !FIXME
      def type_id() end
      ##% type_id= : !FIXME -> !FIXME
      def type_id=(p0) end
      ##% value : !FIXME -> !FIXME
      def value() end
    end

    class Parser
      ##% bufsize : !FIXME -> !FIXME
      def bufsize() end
      ##% bufsize= : !FIXME -> !FIXME
      def bufsize=(p0) end
      ##% input : !FIXME -> !FIXME
      def input() end
      ##% input= : !FIXME -> !FIXME
      def input=(p0) end
      ##% load : (BaseIO or String) -> ?
      def load(*rest) end
      ##% load_documents : (BaseIO or String) {? -> ?} -> NilClass
      def load_documents(*rest) end
      ##% options : !FIXME -> !FIXME
      def options() end
      ##% options= : !FIXME -> !FIXME
      def options=(p0) end
      ##% resolver : !FIXME -> !FIXME
      def resolver() end
      ##% resolver= : !FIXME -> !FIXME
      def resolver=(p0) end
      ##% set_resolver<self> : (::YAML::Syck::Resolver) -> self
      def set_resolver(p0) end
    end

    class Out
      ##% emitter : !FIXME -> !FIXME
      def emitter() end
      ##% emitter= : !FIXME -> !FIXME
      def emitter=(p0) end
      ##% map : !FIXME -> !FIXME
      def map(*rest) end
      ##% scalar : !FIXME -> !FIXME
      def scalar(*rest) end
      ##% seq : !FIXME -> !FIXME
      def seq(*rest) end
    end

    class Emitter
      ##% emit : !FIXME -> !FIXME
      def emit(*rest) end
      ##% level : !FIXME -> !FIXME
      def level() end
      ##% level= : !FIXME -> !FIXME
      def level=(p0) end
      ##% node_export : !FIXME -> !FIXME
      def node_export(p0) end
      ##% reset<self> : () -> self
      ##% reset<self> : (String or BaseIO or Hash<Symbol,!FIXME>) -> self
      def reset(*rest) end
      ##% set_resolver<self> : (::YAML::Syck::Resolver) -> self
      def set_resolver(p0) end
    end

    class BadAlias
      include Comparable
      ##% "<=>" : !FIXME -> !FIXME
      def <=>(p0) end
      ##% name : !FIXME -> !FIXME
      def name() end
      ##% name= : !FIXME -> !FIXME
      def name=(p0) end
    end

    VERSION = "2.3.3"
    class Map < YAML::Syck::Node
      ##% add : !FIXME -> !FIXME
      def add(p0, p1) end
      ##% style= : !FIXME -> !FIXME
      def style=(p0) end
      ##% value= : !FIXME -> !FIXME
      def value=(p0) end
    end

    class MergeKey
    end

    DefaultResolver = ::YAML::Syck::Resolver.new
    class Seq < YAML::Syck::Node
      ##% add : !FIXME -> !FIXME
      def add(p0) end
      ##% style= : !FIXME -> !FIXME
      def style=(p0) end
      ##% value= : !FIXME -> !FIXME
      def value=(p0) end
    end

    class Resolver
      ##% add_type : !FIXME -> !FIXME
      def add_type(p0, p1) end
      ## MikeF: I have no idea what this method does
      ## The C code ignores the argument and returns the empty string
      ##% detect_implicit<t> : t -> String
      def detect_implicit(p0) end
      ##% node_import : !FIXME -> !FIXME
      def node_import(p0) end
      ##% tags : !FIXME -> !FIXME
      def tags() end
      ##% tags= : !FIXME -> !FIXME
      def tags=(p0) end
      ##% tagurize : String -> String
      def tagurize(p0) end
      ## MikeF: not quite sure what the type of the second argument
      ## is here, it seems to be determined dynamically
      ##% transfer : (String,?) -> YAML::DomainType
      def transfer(p0, p1) end
      ##% use_types_at : Hash<String,Class> -> NilClass
      def use_types_at(p0) end
    end

    class Scalar < YAML::Syck::Node
      ##% style= : !FIXME -> !FIXME
      def style=(p0) end
      ##% value= : !FIXME -> !FIXME
      def value=(p0) end
    end

    ##% Syck.compile : !FIXME -> !FIXME
    def Syck.compile(p0) end
  end

  class Object
    ##% class : !FIXME -> !FIXME
    def class() end
    ##% class= : !FIXME -> !FIXME
    def class=(p0) end
    ##% ivars : !FIXME -> !FIXME
    def ivars() end
    ##% ivars= : !FIXME -> !FIXME
    def ivars=(p0) end
    ##% yaml_initialize : !FIXME -> !FIXME
    def yaml_initialize(p0, p1) end
  end

end
