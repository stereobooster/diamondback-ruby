
type ('a,'b) yaml_map = 'a -> Cfg.pos -> 'b list
  (** A shortcut type for the data collected at runtime *)

module type DynamicAnalysis = sig
  (** DynamicAnalysis modules are used to manipulate the Ast/Cfg based
      on data collected at runtime.  Each such module is assumed to be
      paired with a Ruby class defined as DRuby::Profile::[name].
      
      The instrument_{ast,cfg} functions are first passed the original
      source program whose result is then executed.  This
      instrumentation, combined with any online instrumentation
      provided by the Ruby class, is expected to record information
      that maps elements from Domain.t and a source location to (a
      list of) elements from CoDomain.t.

      This data is then passed back to the [DynamicAnalysis] module in
      the form of a partial function with type:
        Domain.t -> pos -> CoDomain.t list
  *)

  (** The type of data collected at runtime by this module *)
  module Domain : Yaml.YType
  module CoDomain : Yaml.YType

  (** The name of this module, which also represents the name of the
      Ruby file to load under druby/profile/ *)
  val name : string
    
  (** instrumentation hooks called before execution *)
  val instrument_ast : Ast.expr list -> Ast.expr list
  val instrument_cfg : Cfg.stmt -> Cfg.stmt

  (** transformation hooks called after the script has been run to
      produce the final cfg *)
  val transform_ast : (Domain.t,CoDomain.t) yaml_map -> Ast.expr list -> Ast.expr list
  val transform_cfg : (Domain.t,CoDomain.t) yaml_map -> Cfg.stmt -> Cfg.stmt

end

(** A list of DynamicAnalysis modules hoisted to the module level.
    Operations on the ast/cfg are performed in head to tail order. *)
module type AnalysisList

(** Constructors for producing modules with type AnalysisList *)
module Cons(X:DynamicAnalysis)(XS:AnalysisList) : AnalysisList
module Singleton(X:DynamicAnalysis) : AnalysisList

module Make : functor (DL : AnalysisList) -> sig
  val profile : File_loader.t -> string -> unit
    (** This function is responsible for performing the actual dynamic
        analysis.  First, the program is executed without modification
        to observe which files are used by the program.  Next, each
        observed file's ast/cfg is folded over the list of [DL]
        modules to produce a single instrumented version of each file.
        These instrumented files are then dumped to /tmp and the
        "main" file is executed.  Finally, the results are stored in
        the file referenced by the [profile_db] configuration
        option.  *)

  val transform : File_loader.t -> string -> Cfg.stmt
    (** Transforms a source program using data gathered from a
        previous profiling run.  The source files are passed to each
        transform_* functions in the [DL] module list to produce a
        final cfg that is then returned. *)

  val run : File_loader.t -> string -> Cfg.stmt
    (** Invokes [profile] followed by [transform] *)
end

(** Dummy module that does not manipulate the ast/cfg in any way.
    Used only for debugging.  *)
module NoOpAnalysis : DynamicAnalysis
