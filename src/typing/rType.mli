
open Utils

type 'a default_list_ = [
   | `Param_Default of 'a * 'a default_list_
   | `Param_Star of 'a
   | `Param_Empty
  ]

type 'a param_typ_ = [
  | `Param_Var
  | `Param_t of 'a * 'a param_typ_
  | `Param_tuple of 'a * 'a * 'a param_typ_
  | 'a default_list_
  ]

and 'a tuple_typ_ = 
  | Array of 'a
  | Tuple_Nil of 'a
  | Tuple_Star of 'a * 'a
  | Tuple_Cons of 'a * 'a * 'a tuple_typ_ Variable.t
  | Tuple_Rest

type top_t

module CG : ConstraintGraph.S

module type TypeKind_S = sig
  type t
  val format : Format.formatter -> t -> unit
  val format_small : Format.formatter -> t -> unit
  val unify : CG.t -> t -> t -> ctx:Log.ctx -> unit
end

module rec KTyp : 
sig
  include TypeKind_S with type t = top_t
    
  val create : Log.ctx -> t
  val format : Format.formatter -> t -> unit
  val format_small : Format.formatter -> t -> unit

  val of_closed : KClass.t -> Log.ctx -> t
  val of_class : KClass.t -> Log.ctx -> t
  val of_tuple : KTuple.t -> Log.ctx -> t
  val of_union : t list -> Log.ctx -> t
  val of_record : KRecord.t -> Log.ctx -> t

  val of_method : KMethod.t -> Log.ctx -> t
    (** create a new open object containing a single method *)
    
  val of_field : KField.t -> Log.ctx -> t
    (** create a new open object containing a single field *)
    
  val of_param : KParam.t -> Log.ctx -> t
    
  val as_tuple : KTyp.t -> KTuple.t option
    
  val as_class : KTyp.t -> KClass.t option
    (** return the class of t iff it is already a class *)
    
  val coerce_class : CG.t -> KTyp.t -> Log.ctx -> KClass.t option
    (** like as_class, but also unifies variables to classes *)

  val splat : CG.t -> KTyp.t -> Log.ctx -> KTyp.t
    (** Simulate Ruby's * operator to coerce a value into an array *)

  val new_instance : CG.t -> ?name:string -> KTyp.t -> Log.ctx -> KTyp.t
  (** [new_instance cg ?name cls ctx] Creates a new 'instance' of the
      object 'cls' as would be done by Ruby's 'new' construct.  If the
      name argument is omitted, this function attempts to determine it
      from the argument 'cls'.  Typically, this argument can be
      ommitted when the 'cls' is a class (e.g. "String.new"), but may
      be supplied (e.g. "X=Object.new" requires X be supplied) *)
    
  val assert_named_class : CG.t -> t -> string -> Log.ctx -> unit
  (** assert that 't' is actually a class with the given name *)

  val instantiate_annotation : CG.t -> t -> Log.ctx -> t
    (** Produces a monomorphic instance of the class [t] where each
        type parameter is instantiated to a unique "witness" type that
        is a subtype of [t], satisfies reflexivity, but is
        incomparable with any other "witness". *)
end
  
and KClass : 
sig
  include TypeKind_S

  val create : Log.ctx -> ?name:string -> ?inst:KClass.t -> 
    ?class_vars:KField.t Utils.StrMap.t Variable.t ->
    ?declared_subs:KClass.t list -> unit -> t

  val instance_class_name : KClass.t -> string option
  val immediate_class_name : KClass.t -> string option

  val get_instance_class : KClass.t -> KClass.t

  val add_parent : CG.t -> parent:KTyp.t -> KClass.t -> ctx:Log.ctx -> unit
  val add_method : CG.t -> KMethod.t -> KClass.t -> Log.ctx -> unit
  val add_constant : CG.t -> string -> KTyp.t -> KClass.t -> Log.ctx -> unit

  val mem_constant : string -> KClass.t -> bool
  val lookup_constant : CG.t -> string -> KClass.t -> Log.ctx -> KTyp.t
  val lookup_inst_var : CG.t -> string -> KClass.t -> Log.ctx -> KTyp.t
  val lookup_class_var : CG.t -> string -> KClass.t -> Log.ctx -> KTyp.t

  val alias_method : CG.t -> exists:string -> link:string -> KClass.t 
    -> KTyp.t -> Log.ctx -> unit

  val module_function : CG.t -> string -> KClass.t -> Log.ctx -> unit

end


and KMethod : 
sig 
  include TypeKind_S
  val create : Log.ctx -> string -> ?func:KFunc.t -> ?block:KBlock.t -> unit -> KMethod.t
  val method_name : t -> string
  val instantiate_annotation : CG.t -> t -> Log.ctx -> t option
    (** Produces a monomorphic instance of the method [t].
        Intersection types are not yet supported, so in that case, we
        return None. *)
end

and KParam : 
sig
  type params = KTyp.t param_typ_
  type t = params Variable.t
  val create : Log.ctx -> KParam.t
  val format : Format.formatter -> t -> unit
  val format_small : Format.formatter -> t -> unit

  type any_list = [
  | `Param_Default of KTyp.t * any_list
  | `Param_Star of KTyp.t
  | `Param_Empty
  | `Param_Var
  | `Param_t of KTyp.t * any_list
  | `Param_tuple of KTyp.t * KTyp.t * any_list
  ]

end

and KFunc : 
sig
  include TypeKind_S
  val create : Log.ctx -> ?self:KTyp.t -> ?args:KParam.t -> ?ret:KTyp.t -> unit -> KFunc.t
  val get_self : t -> KTyp.t
  val get_params : t -> KParam.t
  val get_ret : t -> KTyp.t
end

and KBlock : 
sig
  include TypeKind_S
  val create : Log.ctx -> ?bsig:KFunc.t -> unit -> KBlock.t
  val no_block : Log.ctx -> KBlock.t
  val to_func : CG.t -> KBlock.t -> Log.ctx -> KFunc.t option
end

and KTuple : 
sig
  include TypeKind_S

  val create : Log.ctx -> array:KTyp.t -> KTyp.t list -> KTuple.t
  val of_tuple_list : CG.t -> KTyp.t tuple_typ_ Variable.t -> Log.ctx -> KTuple.t 
end

and KRecord : 
sig
  include TypeKind_S
  val create : Log.ctx -> hash:KTyp.t -> (Cfg.literal * KTyp.t) list
    -> KRecord.t

  val access : getset_obj:KTyp.t -> hash:KTyp.t
    -> (Cfg.literal * top_t) list -> Log.ctx -> KRecord.t
end

and KField  : sig 
  include TypeKind_S
  val create : Log.ctx -> string -> ?t:KTyp.t -> unit -> KField.t
end

module type Constraint_Mod = sig
  module ConKind : TypeKind_S
  val sub_constraint : CG.t -> ConKind.t -> ConKind.t -> Log.ctx -> unit
end

module ConTyp : Constraint_Mod with module ConKind = KTyp
module ConClosed : Constraint_Mod with module ConKind = KClass
module ConMethod : Constraint_Mod with module ConKind = KMethod
module ConSuper : Constraint_Mod with module ConKind = KTyp

module Deprecated : sig
  val get_class : CG.t -> KTyp.t -> Log.ctx -> KClass.t
  val fresh_instance_with_params : CG.t -> KTyp.t -> KTyp.t list -> Log.ctx -> KTyp.t
end
 
module Annotations : sig

  type annot_env
    
  val empty_annot_env : KClass.t -> annot_env
    
  val annot_set_cur_class : annot_env -> KClass.t -> annot_env
    
  val class_of_annotation : CG.t -> annot_env -> Annotation.t
    -> Log.ctx -> Log.pos -> annot_env * KTyp.t
      
  val mt_of_annotation : CG.t -> annot_env -> Annotation.t
    -> Log.ctx -> Log.pos -> annot_env * KMethod.t

  val t_expr_of_annotation : CG.t -> annot_env -> Annotation.t
    -> Log.ctx -> Log.pos -> KTyp.t
end
