open Printf
open Utils
open Config
open Cfg_printer

let ep = Printf.eprintf

exception Unify_Error of Log.ctx * string
exception Promote

module LitMap = Map.Make(struct
                           type t = Cfg.literal 
                           let compare = Pervasives.compare end
                        )

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

type t = typ Variable.t
and top_t = t    
and typ = 
  | Typ_Var  (* monomorphic var *)
  | Typ_PVar (* polymorphic var *)
  | Typ_Union of t list
  | Typ_Open of class_typ Variable.t
  | Typ_Closed of class_typ Variable.t
  | Typ_Tuple of tuple_typ Variable.t
  | Typ_Record of record_typ Variable.t
  | Typ_Forall of poly_env Variable.t * class_typ Variable.t
  | Typ_Inst of t * t list option * t
  | Typ_Param of param_typ Variable.t
  | Typ_Dynamic
  | Typ_Fixme
  | Typ_Witness of int * t
  | Typ_Top

and poly_env = (t * t option) list

and 'a tuple_typ_ = 
  | Array of 'a
  | Tuple_Nil of 'a
  | Tuple_Star of 'a * 'a
  | Tuple_Cons of 'a * 'a * 'a tuple_typ_ Variable.t
  | Tuple_Rest

and tuple_typ = t tuple_typ_

and record_typ = 
  | Hash of t
  | Record of t (*Hash*) * t LitMap.t
  | Record_Access of t (*Get/Set Object*) * t (*Hash*) * t LitMap.t

and class_typ = {
  mutable class_instance : class_typ Variable.t option;
  mutable class_name : string option;
  mutable class_parents : (*class_typ Variable.t StrMap.t*) t list;
  mutable class_constants : field_typ StrMap.t;
  mutable class_methods : method_typ StrMap.t;
  mutable class_vars : field_typ StrMap.t Variable.t;
  mutable inst_vars : field_typ StrMap.t;
  mutable declared_subtypes : class_typ Variable.t list;
}

and field_typ = {
  field_name : string;
  field_typ : t;
}

and method_typ = {
  method_name : string;
  mutable method_kind : method_kind Variable.t
}

and method_kind = 
  | MethodVar
  | MonoMethod of func_sig * block_typ Variable.t
  | InterMethod of (func_sig * block_typ Variable.t) list
  | ForallMethod of poly_env Variable.t * method_kind Variable.t

and param_typ = t param_typ_ 
and default_list = t default_list_

and block_typ = 
  | Block_None
  | Block_Var
  | Block of func_sig

and func_sig = {
  sig_self : typ Variable.t;
  sig_args : param_typ Variable.t;
  sig_ret : typ Variable.t;
}

let top = Variable.create Typ_Top Log.empty

let vcreate msg t ctx = 
  (*Log.fixme "create %s" msg;*)
  Variable.create t ctx

let unify_err f cg t1 t2 ~ctx =
  try f cg t1 t2 ~ctx
  with Unify_Error(ctx, msg) -> Log.err ~ctx "%s" msg

type valid_subtype = [`Sub_Fail | `Sub_Partial of int list | `Sub_Succ ]
type 'a subtype_result = {
  sub_fail : 'a list;
  sub_partial : 'a list * int list;
  sub_succ : 'a list;
}
 
let empty_result = {sub_fail=[];sub_partial=[],[];sub_succ=[]}

let valid_and (v1:valid_subtype) (v2:valid_subtype) : valid_subtype = match v1,v2 with
  | `Sub_Fail, _ | _, `Sub_Fail -> `Sub_Fail
  | `Sub_Partial l1, `Sub_Partial l2 -> `Sub_Partial(l1@l2)
  | `Sub_Partial lst, `Sub_Succ
  | `Sub_Succ, `Sub_Partial lst -> `Sub_Partial lst
  | `Sub_Succ,`Sub_Succ -> `Sub_Succ

let valid_or (v1:valid_subtype) (v2:valid_subtype) : valid_subtype = match v1,v2 with
  | `Sub_Succ, _ | _, `Sub_Succ -> `Sub_Succ
  | `Sub_Partial l1, `Sub_Partial l2 -> `Sub_Partial(l1@l2)
  | `Sub_Partial lst, `Sub_Fail 
  | `Sub_Fail, `Sub_Partial lst -> `Sub_Partial lst
  | `Sub_Fail,`Sub_Fail -> `Sub_Fail

module Kind = struct

  type t = 
    | KTyp of typ Variable.t
    | KSuper of typ Variable.t
    | KInst of typ Variable.t
    | KParam of param_typ Variable.t
    | KClosed of class_typ Variable.t
    | KOpen of class_typ Variable.t
    (*| KTupleList of tuple_list Variable.t*)
    | KTuple of tuple_typ Variable.t
    | KRecord of record_typ Variable.t
    | KMethod of method_typ
    | KBlock of block_typ Variable.t

  let id = function
    | KTyp v -> Variable.vid v
    | KSuper v -> Variable.vid v
    | KInst v -> Variable.vid v
    | KParam v -> Variable.vid v
    | KClosed v -> Variable.vid v
    | KOpen v -> Variable.vid v
    | KTuple v -> Variable.vid v
    | KRecord v -> Variable.vid v
    | KMethod v -> Variable.vid v.method_kind
    | KBlock v -> Variable.vid v

  let ctx = function
    | KTyp v -> Variable.vctx v
    | KSuper v -> Variable.vctx v
    | KInst v -> Variable.vctx v
    | KParam v -> Variable.vctx v
    | KClosed v -> Variable.vctx v
    | KOpen v -> Variable.vctx v
    | KTuple v -> Variable.vctx v
    | KRecord v -> Variable.vctx v
    | KMethod v -> Variable.vctx v.method_kind
    | KBlock v -> Variable.vctx v

  let k_typ v = KTyp v
  let k_super v = KSuper v
  let k_param v = KParam v
  let k_closed v = KClosed v
  let k_open v =  KOpen v
  let k_tuple v = KTuple v
  let k_record v = KRecord v
  let k_method v = KMethod v
  let k_block v = KBlock v
	
  let equal t1 t2 = (id t1) == (id t2)
  let hash t = Hashtbl.hash (id t)
  let compare t1 t2 = Pervasives.compare (id t1) (id t2)
  let to_string = function
    | KTyp _ -> "typ"
    | KSuper _ -> "super"
    | KInst _ -> "inst"
    | KParam _ -> "param"
    | KClosed _ -> "closed"
    | KOpen _ -> "open"
    | KTuple _ -> "tuple"
    | KRecord _ -> "record"
    | KMethod _ -> "method"
    | KBlock _ -> "block"

end

module Constraint = struct 

  type t = {
    ts_ctx : Log.ctx;
    ts_lhs : Kind.t;
    ts_rhs : Kind.t;
    ts_dict : dict;
    ts_origin : origin;
    mutable ts_unsat : bool;
    mutable ts_already_inst : bool;
  } and dict = {
    format : Format.formatter -> unit;
    solve : t -> unit;
    closable : unit -> bool;
    close : t -> t -> Log.ctx -> unit;
    check_remaining_unsat : t -> unit;
    unify : ctx:Log.ctx -> unit;
  }

  and origin = 
    | Source of Log.ctx
    | Derived of t
    | Closure of t * t

  let to_string t = 
    let buf = Buffer.create 128 in
    let ppf = Format.formatter_of_buffer buf in
      t.ts_dict.format ppf;
      Format.pp_print_flush ppf ();
      Printf.sprintf "Constraint: %s" (Buffer.contents buf)

(*    Printf.sprintf "Constraint: lhs %s rhs %s"
      (Kind.to_string t.ts_lhs) (Kind.to_string t.ts_rhs)*)

  let closable t = t.ts_dict.closable ()

  let close t rhs = 
    if closable t then begin
      let ctx = Log.merge t.ts_ctx rhs.ts_ctx in
	t.ts_dict.close t rhs ctx
    end (*else 
      Log.fixme "not closing (%s <= %s) %a @\n and %a"
	(Kind.to_string t.ts_lhs) (Kind.to_string t.ts_rhs)
	(fun ppf () -> t.ts_dict.format ppf) ()
	(fun ppf () -> rhs.ts_dict.format ppf) ()*)

  let rec set_unsat s = 
    s.ts_unsat <- true;
    match s.ts_origin with
      | Source _ -> ()
      | Derived parent -> set_unsat parent
      | Closure(p1,p2) -> set_unsat p1; set_unsat p2

  let rec is_unsat s = match s.ts_origin with
    | Source _ -> s.ts_unsat
    | Derived p -> s.ts_unsat || is_unsat p
    | Closure(p1,p2) -> s.ts_unsat || is_unsat p1 || is_unsat p2

  let solve t = 
    if (is_unsat t) || (Kind.id t.ts_lhs) == (Kind.id t.ts_rhs)
    then ()
    else t.ts_dict.solve t

  let check_remaining_unsat t = t.ts_dict.check_remaining_unsat t
  let collapse e = 
    let ctx = Log.msg "collapsing constraint cycle" e.ts_ctx in
      e.ts_dict.unify ~ctx

  let create k1 k2 dict ~origin ~ctx = 
    {ts_ctx = ctx; ts_origin = origin;
     ts_lhs = k1; ts_rhs = k2; ts_dict = dict;
     ts_unsat = false; ts_already_inst = false;
    }

  let compare t1 t2 = 
    let x1 = Kind.id t1.ts_lhs in
    let y1 = Kind.id t1.ts_rhs in
    let x2 = Kind.id t2.ts_lhs in
    let y2 = Kind.id t2.ts_rhs in
      cmp2 (Pervasives.compare x1 x2) Pervasives.compare y1 y2

  let src s = Kind.id (s.ts_lhs)
  let dst s = Kind.id (s.ts_rhs)

  let solve_closed_map m1 m2 ~push ~err = 
    StrMap.iter
      (fun k v ->
	 try push (StrMap.find k m1) v
	 with Not_found -> err k
      ) m2

  let solve_open_map m1 m2 ~fresh ~push  = 
    StrMap.fold
      (fun k v2 acc ->
	 try push (StrMap.find k acc) v2; acc
	 with Not_found -> 
	   let v1 = fresh v2 in
	   let acc = StrMap.add k v2 acc in
	     push v1 v2;
	     acc
      ) m2 m1

  let ret_unify f cg t1 t2 ~ctx = f cg t1 t2 ~ctx; t1
end

module CG = ConstraintGraph.Make(Constraint)

type variance = ContraVariant | CoVariant

let flip_variance = function
  | ContraVariant -> CoVariant
  | CoVariant -> ContraVariant

class type kind_visitor_class = object('a)
  val ctx : Log.ctx
  method ctx : Log.ctx
  method cgraph : CG.t
  method variance : variance
  method flip_variance : 'a (* functional update *)
  method visit_typ    : typ Variable.t Visitor.visit_method
  method visit_tuple  : tuple_typ Variable.t Visitor.visit_method
  method visit_record : record_typ Variable.t Visitor.visit_method
  method visit_class  : class_typ Variable.t Visitor.visit_method
  method visit_field  : field_typ Visitor.visit_method
  method visit_method : method_typ Visitor.visit_method
  method visit_param  : param_typ Variable.t Visitor.visit_method
  method visit_block  : block_typ Variable.t Visitor.visit_method
  method visit_func   : func_sig Visitor.visit_method
end

class kind_visitor cg_ ctx_ : kind_visitor_class = 
object
  val variance = CoVariant
  val ctx = ctx_
  method ctx = ctx
  method cgraph = cg_
  method variance = variance
  method flip_variance = {<variance=flip_variance variance>}
  method visit_typ _ = Visitor.DoChildren
  method visit_tuple _ = Visitor.DoChildren
  method visit_record _ = Visitor.DoChildren
  method visit_class _ = Visitor.DoChildren
  method visit_field _ = Visitor.DoChildren
  method visit_method _ = Visitor.DoChildren
  method visit_param _ = Visitor.DoChildren
  method visit_block _ = Visitor.DoChildren
  method visit_func _ = Visitor.DoChildren
end

let unify_var cg v1 v2 func = 
  if Variable.same v1 v2 then ()
  else 
    let v' = func ((Variable.deref v1), (Variable.deref v2)) in
      if not (Variable.same v' v1)
      then CG.union_vars cg v1 v';
      if not (Variable.same v' v2)
      then CG.union_vars cg v2 v';
      ()

module type TypeKind_S = sig
  type t
  val format : Format.formatter -> t -> unit
  val format_small : Format.formatter -> t -> unit
  val unify : CG.t -> t -> t -> ctx:Log.ctx -> unit
end

module type Kind_S = sig
  include TypeKind_S

  type create_sig
  val create : Log.ctx -> create_sig

  val visit : kind_visitor -> t -> t
  val visit_children : kind_visitor -> t -> t
end

module type Constraint_Mod = sig
  module ConKind : TypeKind_S
  val sub_constraint : CG.t -> ConKind.t -> ConKind.t -> Log.ctx -> unit
end

module type Constraint_S = sig
  include Constraint_Mod
  val add_constraint: CG.t -> ConKind.t -> ConKind.t
    -> origin:Constraint.origin -> ctx:Log.ctx -> unit
  val valid_subtype: CG.t -> ConKind.t -> ConKind.t -> valid_subtype
end

module type Constraint_Shell = sig
  include Constraint_S
  val solve : CG.t -> ConKind.t -> ConKind.t -> Constraint.t -> unit
  val close : CG.t -> Constraint.t -> Constraint.t -> Log.ctx -> unit
  val check_remaining_unsat : CG.t -> ConKind.t -> ConKind.t -> Constraint.t -> unit
  val closable : ConKind.t -> ConKind.t -> unit -> bool
  val lhs_kind : ConKind.t -> Kind.t
  val rhs_kind : ConKind.t -> Kind.t
end

module Build_Constraint(C : Constraint_Shell) = struct
  let format t1 t2 ppf = 
    Format.fprintf ppf "%a <= %a" C.ConKind.format t1 C.ConKind.format t2
      
  let add_constraint cg t1 t2 ~origin ~ctx = 
    let dict = {Constraint.close = C.close cg;
		solve = C.solve cg t1 t2;
		check_remaining_unsat = C.check_remaining_unsat cg t1 t2;
		unify = C.ConKind.unify cg t1 t2;
		format = format t1 t2;
                closable = C.closable t1 t2;
	       }
    in
    let c = Constraint.create (C.lhs_kind t1) (C.rhs_kind t2) 
      dict ~origin ~ctx
    in
      (*Log.fixme "added con: %s %a <= %a" (Constraint.to_string c)
        C.ConKind.format t1 C.ConKind.format t2;*)
      CG.add_edge cg c

  let sub_constraint cg t1 t2 ctx = 
    add_constraint cg t1 t2 ~origin:(Constraint.Source ctx) ~ctx
end

let fmtf = Format.fprintf 
let fvar = Variable.format_var

module rec KTyp : sig
  include Kind_S with type t = typ Variable.t
		 and type create_sig = t
  val of_closed : KClass.t -> Log.ctx -> KTyp.t
  val of_class : KClass.t -> Log.ctx -> KTyp.t
  val of_tuple : KTuple.t -> Log.ctx -> KTyp.t
  val of_union : KTyp.t list -> Log.ctx -> KTyp.t
  val of_record : KRecord.t -> Log.ctx -> KTyp.t
  val of_method : KMethod.t -> Log.ctx -> KTyp.t
  val of_field : KField.t -> Log.ctx -> KTyp.t
  val of_param : KParam.t -> Log.ctx -> KTyp.t

  val as_tuple : KTyp.t -> KTuple.t option
    (* return the class of t iff it is already a class *)
  val as_class : KTyp.t -> KClass.t option
    (* like as_class, but also unifies variables to classes *)
  val coerce_class : CG.t -> KTyp.t -> Log.ctx -> KClass.t option

  val new_instance : CG.t -> ?name:string -> KTyp.t -> Log.ctx -> KTyp.t
    
  (** assert that 't' is actually a class with the given name *)
  val assert_named_class : CG.t -> t -> string -> Log.ctx -> unit

  (* perform the splat action, like "*e" *)
  val splat : CG.t -> KTyp.t -> Log.ctx -> KTyp.t

  val visit_poly_env : kind_visitor -> poly_env Variable.t -> poly_env Variable.t
  val unify_poly_env : CG.t -> poly_env Variable.t -> poly_env Variable.t
    -> Log.ctx -> unit
  val unify_bound : CG.t -> t option -> t option -> Log.ctx -> t option

  val instantiate_annotation : CG.t -> t -> Log.ctx -> t

end = struct
  type t = top_t
  type create_sig = t

  let create ctx = Variable.create Typ_Var ctx

  let of_closed closed ctx = Variable.create (Typ_Closed closed) ctx
  let of_class clazz ctx = Variable.create (Typ_Open clazz) ctx
  let of_tuple tup ctx = Variable.create (Typ_Tuple tup) ctx 
  let of_union lst ctx = Variable.create (Typ_Union lst) ctx
  let of_record rcd ctx = Variable.create (Typ_Record rcd) ctx 

  let as_tuple t = match Variable.deref t with
    | Typ_Tuple tup -> Some tup
    | _ -> None

  let as_class t =
    match Variable.deref t with
      | Typ_Forall(_,cls) | Typ_Closed cls
      | Typ_Open cls -> Some cls
      | _ -> None

  let rec coerce_class cg t ctx =
    match Variable.deref t with
      | Typ_Forall(_,cls) | Typ_Closed cls
      | Typ_Open cls -> Some cls
      | Typ_Var -> 
          let cls = KClass.create ctx () in
          let t' = of_class cls ctx in
            KTyp.unify cg t t' ctx;
            Some cls
      | Typ_Inst(_,_,tr) -> coerce_class cg tr ctx
      | _ -> None

  let rec format_small ppf t = match Variable.deref t with
    | Typ_Top -> fmtf ppf "Top"
    | Typ_Witness(i,_) -> fmtf ppf "Wit(%d)" i
    | Typ_Var -> fmtf ppf "[%d]Var" (Variable.vid t)
    | Typ_PVar -> fmtf ppf "[%d]PVar" (Variable.vid t)
    | Typ_Union ts -> fmtf ppf "UNION(%a)" (format_comma_list format_small) ts
    | Typ_Open o -> fmtf ppf "[%d]Obj(%a)" (Variable.vid t) KClass.format_small o
    | Typ_Closed ecv -> 
	(*fmtf ppf "[%d]Closed(%a)" (Variable.vid t)*)
	KClass.format_small ppf ecv 

    | Typ_Tuple tup -> fmtf ppf "Tuple(%a)" KTuple.format_small tup
    | Typ_Record r -> fmtf ppf "Record(%a)" KRecord.format_small r
    | Typ_Forall(vmap,ec) -> 
	fmtf ppf "forall <...>. %a" KClass.format_small ec
    | Typ_Inst(p,tlo,tr) -> fmtf ppf "[%d]inst(%d,@[<h><%a>@],%d)"
        (Variable.vid t) (Variable.vid p)
          (format_option (format_comma_list (fun ppf t -> fmtf ppf "%d" (Variable.vid t))))
          tlo
          (Variable.vid tr)
    | Typ_Param pl -> fmtf ppf "^([%d]%a)" (Variable.vid pl) KParam.format_small pl
    | Typ_Dynamic -> fmtf ppf "<Dynamic>"
    | Typ_Fixme -> fmtf ppf "<Fixme>"
	
  let rec format ppf (t:typ Variable.t) =
    Format.pp_set_max_boxes ppf 100; 
    (* fail-safe to prevent stack overflow on rec types *)
    if conf.debug_level > 10 && not (Format.pp_over_max_boxes ppf ())
    then fvar format_typ ppf t
    else if Format.pp_over_max_boxes ppf ()
    then fmtf ppf "..."
    else format_small ppf t
      
  and format_typ ppf typ = match typ with
    | Typ_Top -> fmtf ppf "Top"
    | Typ_Var -> fmtf ppf "Var"
    | Typ_PVar -> fmtf ppf "PVar"
    | Typ_Witness(i,_) -> fmtf ppf "Wit(%d)" i
    | Typ_Union ts -> 
	fmtf ppf "@[UNION(@[%a@])@]"
	  (format_comma_list  format_small) ts
	  
    | Typ_Open v -> KClass.format ppf v
	  
    | Typ_Closed v -> KClass.format ppf v
	  
    | Typ_Tuple tup -> KTuple.format ppf tup
	  
    | Typ_Record r -> KRecord.format ppf r
	  
    | Typ_Forall(vmap,t) -> 
	fmtf ppf "@[forall %a. %a @]"
	  (format_comma_list
	     (fun ppf (t1,t2) -> format_small ppf t1)
	  ) (Variable.deref vmap)
	  KClass.format t
    | Typ_Inst(t,tlo,tr) -> 
	fmtf ppf "@[inst(%a,%a,%a)@]"
	  format t
	  (format_option @< format_comma_list @< format) tlo
	  format tr
    | Typ_Param pl -> fmtf ppf "^(%a)" KParam.format pl
    | Typ_Dynamic -> fmtf ppf "<Dynamic>"
    | Typ_Fixme -> fmtf ppf "<Fixme>"

  let rec unify_exn cgraph t1 t2 ~ctx = 
    (*Log.note "unify: %a <=> %a" format_small t1 format_small t2;*)
    unify_var cgraph t1 t2 @< function
    | Typ_Var, _ -> t2
    | _, Typ_Var -> t1

    | Typ_PVar, Typ_PVar -> t1

    | Typ_PVar, _ -> 
        Log.err ~ctx "can't unify %a with polymorphic type variable" KTyp.format t2;
        t2
    | _, Typ_PVar ->
        Log.err ~ctx "can't unify %a with polymorphic type variable" KTyp.format t1;
        t1

    | Typ_Top, Typ_Top -> t1

    | Typ_Top, _
    | _, Typ_Top -> raise (Unify_Error(ctx, "unify with top"))

    | Typ_Witness(i,_), Typ_Witness(j,_) -> 
        if i == j then t1
        else raise (Unify_Error(ctx,"Unifying two witness types"))

    | Typ_Witness _, _
    | _, Typ_Witness _ ->
        raise (Unify_Error(ctx, "unify with witness type"))

    | Typ_Dynamic, _ -> Log.fixme "should I unify type dynamic?"; t2
    | _, Typ_Dynamic -> Log.fixme "should I unify type dynamic?"; t1
	
    | Typ_Fixme, _ -> Log.fixme ~ctx "!FIXME type in unify"; t2
    | _, Typ_Fixme -> Log.fixme ~ctx "!FIXME type in unify"; t1

    | Typ_Union lst1, Typ_Union lst2 ->
	CG.union_vars cgraph t2 t1;
	begin try
	  List.iter2 (unify_exn cgraph ~ctx) lst1 lst2;
	  t1
	with Invalid_argument _ ->
	  raise (Unify_Error(ctx,"unify unions with different sizes"))
	end
    | Typ_Union lst, _ ->
        CG.union_vars cgraph t1 t2;
        List.iter (unify_exn cgraph ~ctx t2) lst;
        t2

    | _, Typ_Union lst ->
        CG.union_vars cgraph t2 t1;
        List.iter (unify_exn cgraph ~ctx t1) lst;
        t1
	  
    | Typ_Closed c1, Typ_Closed c2 -> 
	CG.union_vars cgraph t2 t1;
	KClass.unify cgraph c1 c2 ~ctx; 
	t1
	    
    | Typ_Open cl1, Typ_Open cl2 -> 
	CG.union_vars cgraph t2 t1;
	KClass.unify cgraph cl1 cl2 ~ctx; t1

    | Typ_Open op, Typ_Closed cl -> 
	CG.union_vars cgraph t1 t2;
	KClass.unify cgraph op cl ~ctx; t2
    | Typ_Closed cl, Typ_Open op -> 
	CG.union_vars cgraph t2 t1;
	KClass.unify cgraph op cl ~ctx; t1

    | Typ_Tuple tup1, Typ_Tuple tup2 -> 
	CG.union_vars cgraph t2 t1;
	KTuple.unify cgraph tup1 tup2 ~ctx; t1

    | Typ_Forall(env1,subt1), Typ_Forall(env2,subt2) ->
	CG.union_vars cgraph t2 t1;
	if env1 != env2 
	then begin
	  let msg = sprintf "foralls with different envs: %s %s"
	    (format_to_string format_small t1) (format_to_string format_small t2)
	  in
	    raise (Unify_Error(ctx,msg))
	end;
	KClass.unify cgraph subt1 subt2 ~ctx;
	t1

    | Typ_Forall(env,cls1), Typ_Closed cls2 ->
        CG.union_vars cgraph t2 t1;
        KClass.unify cgraph cls1 cls2 ~ctx;
	(*Log.fixme ~ctx "unify forall/closed";*)
	t1

    | Typ_Closed cls1, Typ_Forall(env,cls2) ->
        CG.union_vars cgraph t1 t2;
        KClass.unify cgraph cls1 cls2 ~ctx;
	(*Log.fixme ~ctx "unify closed/forall";*)
	t2

    | Typ_Param f1, Typ_Param f2 ->
        CG.union_vars cgraph t2 t1;
	KParam.unify cgraph f1 f2 ~ctx;
	t1

    | Typ_Param _, _ 
    | _, Typ_Param _ ->
	Log.fatal ctx "can't unify function type with other toplevel-typ"

    | Typ_Inst(p1,args1,tr1), Typ_Inst(p2,args2,tr2) ->
	if (p1 != p2)
	then raise (Unify_Error(ctx, "foralls different for inst type??"));
	CG.union_vars cgraph t2 t1;
	unify_exn cgraph tr1 tr2 ctx;
	begin match args1, args2 with
	  | None, None -> t1
	  | Some _, None -> t1
	  | None, Some _ -> t2
	  | Some l1, Some l2 ->
	      try
		List.iter2 (fun t1 t2 -> unify_exn cgraph t1 t2 ctx) l1 l2;
		t1
	      with Invalid_argument _ ->
		let msg = "different length inst lists for inst type??" in
		  raise (Unify_Error(ctx, msg))
	end

    | Typ_Inst(_,_,tr) , _ ->
	CG.union_vars cgraph t2 tr;
	unify_exn cgraph tr t2 ctx;
	t1

    | _, Typ_Inst(_,_,tr) ->
	CG.union_vars cgraph t1 tr;
	unify_exn cgraph t1 tr ctx;
	t2

    | Typ_Record d1, Typ_Record d2 ->
	Log.fatal Log.empty "unify dictionaries"

    | Typ_Tuple tup, Typ_Closed cl
    | Typ_Closed cl, Typ_Tuple tup ->
        let tup_t,arr_t = match Variable.deref t1 with
            Typ_Tuple _ -> t1, t2
          | _ -> t2, t1
        in
        begin match KClass.instance_class_name cl with
          | Some "Array" ->
              let tup_arr = KTuple.promote cgraph tup ctx in
                CG.union_vars cgraph tup_t arr_t ;
                unify_exn cgraph tup_arr arr_t ctx;
                arr_t
          | _ ->
              let msg = sprintf "can't unify tuple with %s" 
	        (format_to_string KClass.format cl)
	      in
	        raise (Unify_Error(ctx,msg))
        end

    | (Typ_Record _ | Typ_Tuple _ | Typ_Forall _ 
      | Typ_Closed _ | Typ_Open _), _ -> 
	let msg = 
	  sprintf "unification error:\n %s\n   <>\n %s" 
	    (format_to_string format_small t1) (format_to_string format_small t2)
	in
	  raise (Unify_Error(ctx,msg))

  let unify cgraph t1 t2 ~ctx = 
    unify_err unify_exn cgraph t1 t2 ~ctx

  let unify_bound cg b1 b2 ctx = match b1, b2 with
    | Some x, Some y -> KTyp.unify cg x y ctx; b1
    | Some x, None -> b1
    | None, Some y -> b2
    | None, None -> None

  let map2_preserve map2 f t1 t2 = 
    let changed = ref false in
    let t' = 
      map2 (fun v1 v2 ->
              let v' = f v1 v2 in
                if v' != v1
                then changed := true;
                v'
        ) t1 t2
    in if !changed then t' else t1

  let unify_poly_env cg env1u env2u ctx = 
    unify_var cg env1u env2u @< fun (env1,env2) ->
      CG.union_vars cg env2u env1u;
      begin try
	let env' = map2_preserve List.map2
          (fun ((t1,b1) as orig) (t2,b2) -> 
             KTyp.unify cg t1 t2 ctx;
             let b' = unify_bound cg b1 b2 ctx in
               if b' != b1 then (t1,b')
               else orig
          ) env1 env2
        in
          if env1 != env' then Variable.create env' ctx
          else env1u
      with Invalid_argument _ ->
        Log.err ~ctx "unifying polymorphic environments of different sizes";
        env1u
      end

  let of_method mt ctx = 
    let o = {class_name = None;
	     class_instance = None;
	     class_parents = [];
	     class_methods = StrMap.add mt.method_name mt StrMap.empty;
	     inst_vars = StrMap.empty;
	     class_vars = Variable.create StrMap.empty ctx;
	     class_constants = StrMap.empty;
             declared_subtypes = [];
	    }
    in Variable.create (Typ_Open (Variable.create o ctx)) ctx

  let of_field ft ctx = 
    let o = {class_name = None;
	     class_instance = None;
	     class_parents = [];
	     class_methods = StrMap.empty;
	     inst_vars = StrMap.add ft.field_name ft StrMap.empty;
	     class_vars = Variable.create StrMap.empty ctx;
	     class_constants = StrMap.empty;
             declared_subtypes = [];
	    }
    in Variable.create (Typ_Open (Variable.create o ctx)) ctx

  let of_param param ctx = Variable.create (Typ_Param param) ctx

  let assert_named_class cg t name ctx =
    match Variable.deref t with
      | Typ_Forall(_,ecv)
      | Typ_Closed ecv ->
	  let ec = Variable.deref ecv in
	    begin match ec.class_name with
	      | None -> ec.class_name <- Some name
	      | Some s -> 
		  if String.compare s name <> 0
		  then Log.err ~ctx
		    "expecting class with name %s, but got %s instead"
		    name s
	    end
      | Typ_Var -> 
	  let c = KClass.create ctx ~name () in
	  let t' = Variable.create (Typ_Closed c) ctx  in
	    unify cg t t' ~ctx

      | _ -> 
	  Log.err ~ctx "expecting the class %s, but got %a instead"
	    name format t
	
  let new_instance cg ?name parent ctx = 
    match Variable.deref parent with
      | Typ_Top -> Log.err ~ctx "can't instantiate top"; create ctx
      | Typ_Fixme -> Log.fixme ~ctx "instance of !FIXME"; create ctx
      | Typ_Witness _ -> 
          Log.err ~ctx "can't instantiate this type, it should be parametric";
          create ctx
      | Typ_Dynamic -> create (Log.in_ctx ctx "instance of dynamic type")
      | Typ_Inst _ | Typ_Var | Typ_PVar | Typ_Forall _ -> 
	  let tr = KTyp.create ctx in
	  let t' = Variable.create (Typ_Inst(parent,None,tr)) ctx in
	    ConInstance.add_constraint cg parent t' 
              ~origin:(Constraint.Source ctx) ~ctx;
	    t'
	  
      | Typ_Open pcls | Typ_Closed pcls ->
	  let iname = match name with
	    | Some x -> name
	    | None -> match KClass.immediate_class_name pcls with
		| None -> None
		| Some n -> Some n
	  in
	  let cls = KClass.create ctx () in
	  let () = KClass.add_parent cg ~parent:parent cls ~ctx in
	  let inst = KClass.create ctx ?name:iname ~inst:cls () in
            of_closed inst ctx
	      
      | Typ_Record _ | Typ_Union _ | Typ_Tuple _ | Typ_Param _ ->
	  Log.err ~ctx "can't instantiate non-class type: %a" 
	    KTyp.format parent;
	  create ctx

  let obj_with_splat_method splatted ctx = 
    let ret = KTyp.create ctx in
    let args = Variable.create (`Param_t(splatted, `Param_Empty)) ctx in
    let func = KFunc.create ~self:splatted ~args ~ret ctx () in
    let block = KBlock.no_block ctx in
    let mt = KMethod.create ctx "__splat" ~func ~block () in
    let clz = KTyp.of_method mt ctx in
      clz, ret

  let splat cg t ctx = 
    match Variable.deref t with
      | Typ_Tuple tup -> t
      | _ -> 
          let splat_t,ret = obj_with_splat_method t ctx in
            ConTyp.sub_constraint cg t splat_t ctx;
            ret

  let rec visit vtor t = 
    Visitor.visit vtor#visit_typ t (visit_children vtor)

  and visit_children vtor t = match Variable.deref t with
    | Typ_Var -> t
    | Typ_PVar -> t
    | Typ_Union ts ->
	let ts' = map_preserve List.map (visit vtor) ts in
	  if ts != ts'
	  then vcreate "t union" (Typ_Union ts') vtor#ctx
	  else t
	    
    | Typ_Closed c -> 
	let c' = KClass.visit vtor c in
	  if c != c' 
	  then vcreate "t closed" (Typ_Closed c') vtor#ctx
	  else t

    | Typ_Open c -> 
	let c' = KClass.visit vtor c in
	  if c != c' 
	  then vcreate "t open" (Typ_Open c') vtor#ctx
	  else t

    | Typ_Top
    | Typ_Witness _ -> t

    | Typ_Tuple tup ->
        let tup' = KTuple.visit vtor tup in
          if tup != tup'
          then vcreate "t tuple" (Typ_Tuple tup') vtor#ctx
          else t

    | Typ_Record dict ->
        let dict' = KRecord.visit vtor dict in
          if dict != dict'
          then vcreate "t dict" (Typ_Record dict') vtor#ctx
          else t

    | Typ_Inst(p,args_o,tr) -> 
        let p' = visit vtor p in
        let args_o' = map_opt_preserve (map_preserve List.map (visit vtor)) args_o in
        let tr' = visit vtor tr in
          if p != p' || args_o != args_o || tr != tr' 
          then vcreate "t inst" (Typ_Inst(p',args_o',tr')) vtor#ctx
          else t

    | Typ_Forall(fenv,clz) -> 
        let fenv' = visit_poly_env vtor fenv in
        let clz' = KClass.visit vtor clz in
          if clz != clz' || fenv != fenv'
          then vcreate "t forall" (Typ_Forall(fenv',clz')) vtor#ctx
          else t

    | Typ_Param f -> 
        (* Contra *)
	let f' = KParam.visit (vtor#flip_variance) f in
	  if f != f' 
	  then vcreate "t func" (Typ_Param f') vtor#ctx
	  else t

    | Typ_Dynamic -> t
    | Typ_Fixme -> t

  and visit_poly_env vtor fenv = 
    let env1 = Variable.deref fenv in
    let env2 = map_preserve List.map
      (fun ((t,bound) as pair) -> match bound with
         | None -> pair
         | Some b -> 
             let b' = visit vtor b in
               if b != b'
               then (t,Some b')
               else pair
      ) env1
    in
      if env1 != env2 then Variable.create env2 vtor#ctx
      else fenv

  let instantiate_annotation cg typ ctx = match Variable.deref typ with
    | Typ_Forall(env,cls) ->
        let cls' = KSubstitution.class_witness cg (Variable.deref env) cls ctx in
        let t = of_closed cls' ctx in
        let tr = KTyp.create ctx in
        let inst = Variable.create (Typ_Inst(t,None,tr)) ctx in
          ConInstance.add_constraint cg t inst ~ctx ~origin:(Constraint.Source ctx);
          inst

    | _ -> Log.fatal ctx "can't instantiate non-forall type"


end and KTuple : 
sig
  include Kind_S  with type t = tuple_typ Variable.t
		  and type create_sig = array:t -> t list -> tuple_typ Variable.t

  val of_tuple_list : CG.t -> tuple_typ Variable.t -> Log.ctx -> KTuple.t 
    
  val promote : CG.t -> KTuple.t -> Log.ctx -> KTyp.t
end = 
struct
  type t = tuple_typ Variable.t

  type create_sig = array:KTyp.t -> KTyp.t list -> KTuple.t

  let tuple_typ_of_list arr l ctx = 
    List.fold_left (fun acc x -> Variable.create (Tuple_Cons(arr,x,acc)) ctx) 
      (Variable.create (Tuple_Nil arr) ctx) (List.rev l)

  let create ctx ~array typs = 
    tuple_typ_of_list array typs ctx

  let of_tuple_list cg tup ctx = tup

  let rec promote cg t ctx = 
    let rec work t = match Variable.deref t with
    | Array typ -> typ
    | Tuple_Rest -> KTyp.create ctx
    | Tuple_Nil arr
    | Tuple_Star(arr,_) ->
        let ctx = Log.in_ctx ctx "promoting type" in
        let pro = Variable.create (Array arr) ctx in
          CG.union_vars cg t pro;
          arr

    | Tuple_Cons(arr,_,rest) -> 
        let ctx = Log.in_ctx ctx "promoting type" in
        let pro = Variable.create (Array arr) ctx in
          CG.union_vars cg t pro;
          ignore(work rest);
          arr
    in work t

  let rec format_tuple_ fmt ppf lst = match Variable.deref lst with
    | Array t -> fmtf ppf "Tup_as_Array(%a)" fmt t
    | Tuple_Nil _ -> ()
    | Tuple_Cons(_,t,ts) -> 
	fmtf ppf "%a, %a" fmt t (format_tuple_ fmt) ts
    | Tuple_Star(_,t) -> fmtf ppf "*(%a)" fmt t
    | Tuple_Rest -> Format.pp_print_string ppf "..."

  let format_small ppf t = 
    format_tuple_ KTyp.format_small ppf t
	
  let format ppf t =
    fmtf ppf "@[(%a)@]" (format_tuple_ KTyp.format) t

  let unify cgraph t1 t2 ~ctx  = unify_var cgraph t1 t2 @< function
    | Array a1, Array a2
    | Tuple_Nil a1, Tuple_Nil a2 -> 
        CG.union_vars cgraph t2 t1;
        KTyp.unify cgraph a1 a2 ctx;
        t1
          
    | Tuple_Star(a1,s1), Tuple_Star(a2,s2) ->
        CG.union_vars cgraph t2 t1;
	KTyp.unify cgraph a1 a2 ~ctx;
	KTyp.unify cgraph s1 s2 ~ctx;
	t1
	  
    | Tuple_Cons(a1,x,xs), Tuple_Cons(a2,y,ys) ->
        CG.union_vars cgraph t2 t1;
	KTyp.unify cgraph a1 a2 ~ctx;
	KTyp.unify cgraph x y ~ctx;
        KTuple.unify cgraph xs ys ctx;
	t1

    | _ -> raise Promote
          
  let rec visit vtor t = 
    Visitor.visit vtor#visit_tuple t (visit_children vtor)
  and visit_children vtor t = match Variable.deref t with
    | Array arr ->
        let arr' = KTyp.visit vtor arr in
          if arr != arr' then Variable.create (Array arr') vtor#ctx else t

    | Tuple_Nil arr -> 
        let arr' = KTyp.visit vtor arr in
          if arr == arr' then t
          else Variable.create (Tuple_Nil arr') vtor#ctx

    | Tuple_Star(arr, s) ->
        let arr' = KTyp.visit vtor arr in
        let s' = KTyp.visit vtor s in
          if arr == arr' && s == s' 
          then t
          else Variable.create (Tuple_Star(arr',s')) vtor#ctx

    | Tuple_Cons(arr,x,rest) -> 
        let arr' = KTyp.visit vtor arr in
        let x' = KTyp.visit vtor x in
        let rest' = visit_children vtor rest in
          if arr == arr' && x == x' && rest == rest' 
          then t 
          else Variable.create (Tuple_Cons(arr',x',rest')) vtor#ctx

    | Tuple_Rest -> t

end and KRecord : 
sig
  include Kind_S with type t = record_typ Variable.t
                 and type create_sig = hash:top_t -> (Cfg.literal * top_t) list
                   -> record_typ Variable.t

  val access : getset_obj:KTyp.t -> hash:KTyp.t
    -> (Cfg.literal * top_t) list -> Log.ctx -> KRecord.t

  val to_tuple : CG.t -> array:KTyp.t -> KTyp.t LitMap.t -> Log.ctx -> KTuple.t option
  val promote : CG.t -> KRecord.t -> Log.ctx -> KTyp.t
end = struct
  type t = record_typ Variable.t
  type create_sig = hash:KTyp.t -> (Cfg.literal * KTyp.t) list -> t

  let create ctx ~hash lst = 
    let map = List.fold_left (fun acc (k,v) -> LitMap.add k v acc) LitMap.empty lst in
      Variable.create (Record (hash,map)) ctx

  let access ~getset_obj ~hash lst ctx = 
    let map = List.fold_left (fun acc (k,v) -> LitMap.add k v acc) 
      LitMap.empty lst 
    in
      Variable.create (Record_Access (getset_obj,hash,map)) ctx

  let promote cg t ctx = match Variable.deref t with
    | Hash typ -> typ
    | Record(hsh,_) 
    | Record_Access(_,hsh,_) ->
        let ctx = Log.in_ctx ctx "promoting type" in
        let pro = Variable.create (Hash hsh) ctx in
          CG.union_vars cg t pro;
          hsh

  let to_tuple cg ~array map ctx = 
    let max_o = 
      LitMap.fold
        (fun k v acc -> match acc, k with
           | Some max, `Lit_FixNum i -> 
               if i > max then Some i else acc
           | _ -> None
        ) map (Some (-1))
    in
    let rec work idx acc = 
      if idx < 0 then acc
      else 
        let v = try LitMap.find (`Lit_FixNum idx) map
        with Not_found -> KTyp.create ctx 
        in
          work (idx-1) (v::acc)
    in
      match max_o with
        | None -> None
        | Some -1 -> None
        | Some max ->
            let lst = work max [] in
            let tup = 
              List.fold_left
                (fun acc x -> Variable.create (Tuple_Cons(array,x,acc)) ctx) 
                (Variable.create (Tuple_Rest) ctx) (List.rev lst)
            in
              Some tup

  let format_ f ppf t = match Variable.deref t with
    | Hash h -> f ppf h
    | Record_Access(_,_,map)
    | Record(_,map) -> 
        fmtf ppf "Record<@[<hov>%a@]>"
          (format_map LitMap.iter CodePrinter.format_literal KTyp.format) map

  let format_small ppf t = format_ KTyp.format_small ppf t
  let format ppf t = format_ KTyp.format ppf t

  let unify_litmap cg map1 map2 ctx = 
    LitMap.fold
      (fun k v acc ->
         try
           let v' = LitMap.find k acc in
           let joined = Constraint.ret_unify KTyp.unify cg v v' ~ctx in
             LitMap.add k joined acc
         with Not_found -> LitMap.add k v acc
      ) map2 map1
      
  let unify cg t1 t2 ~ctx = 
    let ctx = Log.in_ctx ctx "unifying records %a and %a" 
      KRecord.format_small t1 KRecord.format_small t2
    in unify_var cg t1 t2 @< function
      | Hash h1, Hash h2 -> 
          CG.union_vars cg t2 t1;
          KTyp.unify cg h1 h2 ~ctx;
          t1

      | Hash h1, (Record(hsh,_)|Record_Access(_,hsh,_)) -> 
          let h2 = KRecord.promote cg t2 ctx in
            CG.union_vars cg t2 t1;
            KTyp.unify cg h1 h2 ~ctx;
            t1

      | (Record(hsh,_)|Record_Access(_,hsh,_)), Hash h2 -> 
          let h1 = KRecord.promote cg t1 ctx in
            CG.union_vars cg t1 t2;
            KTyp.unify cg h1 h2 ~ctx;
            t2

      | Record _, Record_Access _
      | Record_Access _, Record _ ->
          let p1 = KRecord.promote cg t1 ctx in
          let p2 = KRecord.promote cg t2 ctx in
            CG.union_vars cg t1 t2;
            KTyp.unify cg p1 p2 ctx;
            t2

      | Record_Access(obj1,hsh1,map1), Record_Access(obj2,hsh2,map2) ->
          let map' = unify_litmap cg map1 map2 ctx in
            CG.union_vars cg t2 t1;
            KTyp.unify cg obj1 obj2 ctx;
            KTyp.unify cg hsh1 hsh2 ctx;
            Variable.create (Record_Access(obj1,hsh1,map')) ctx
              
      | Record(h1,map1), Record(h2,map2) ->
          CG.union_vars cg t2 t1;
          KTyp.unify cg h1 h2 ctx;
          let map' = unify_litmap cg map1 map2 ctx in
            Variable.create (Record(h1,map')) ctx

  let rec visit vtor t = 
    Visitor.visit vtor#visit_record t (visit_children vtor)
  and visit_children vtor t = match Variable.deref t with
    | Hash h -> 
        let h' = KTyp.visit vtor h in
          if h == h' then t else Variable.create (Hash h') vtor#ctx
    | Record_Access(obj,hsh,map) -> 
        let obj' = KTyp.visit vtor obj in
        let hsh' = KTyp.visit vtor hsh in
        let map' = map_preserve LitMap.map (KTyp.visit vtor) map in
          if obj == obj' && hsh == hsh' && map == map' then t
          else Variable.create (Record_Access(obj',hsh',map')) vtor#ctx
    | Record(hsh,map) -> 
        let hsh' = KTyp.visit vtor hsh in
        let map' = map_preserve LitMap.map (KTyp.visit vtor) map in
          if hsh == hsh' && map == map' then t
          else Variable.create (Record(hsh',map')) vtor#ctx

end and KClass : 
sig
  include Kind_S with type t=class_typ Variable.t
		 and type create_sig = ?name:string -> ?inst:class_typ Variable.t -> 
  ?class_vars:field_typ Utils.StrMap.t Variable.t -> 
  ?declared_subs:class_typ Variable.t list -> unit -> class_typ Variable.t

  val instance_class_name : KClass.t -> string option
  val immediate_class_name : KClass.t -> string option
  val get_instance_class : KClass.t -> KClass.t
  val has_instance_class : KClass.t -> bool
    
  val add_parent : CG.t -> parent:KTyp.t -> KClass.t -> ctx:Log.ctx -> unit
  val add_method : CG.t -> KMethod.t -> KClass.t -> Log.ctx -> unit
  val add_constant : CG.t -> string -> KTyp.t -> KClass.t -> Log.ctx -> unit
  val mem_constant : string -> KClass.t -> bool
  val lookup_constant : CG.t -> string -> KClass.t -> Log.ctx -> KTyp.t
  val lookup_inst_var : CG.t -> string -> KClass.t -> Log.ctx -> KTyp.t
  val lookup_class_inst_var : CG.t -> string -> KClass.t -> Log.ctx -> KTyp.t
  val lookup_class_var : CG.t -> string -> KClass.t -> Log.ctx -> KTyp.t
  val alias_method : CG.t -> exists:string -> link:string -> KClass.t
    -> KTyp.t -> Log.ctx -> unit
  val module_function : CG.t -> string -> KClass.t -> Log.ctx -> unit

  val all_methods_respond : KClass.t -> KMethod.t StrMap.t
  val all_fields : KClass.t -> KField.t StrMap.t

  val unify_class_vars : CG.t -> KField.t StrMap.t Variable.t -> 
    KField.t StrMap.t Variable.t -> ctx:Log.ctx -> unit
  val over_up : KClass.t -> KClass.t option

end = struct

  type typ = class_typ
  type t = typ Variable.t
  type create_sig = ?name:string -> ?inst:KClass.t
      -> ?class_vars:KField.t Utils.StrMap.t Variable.t
      -> ?declared_subs:KClass.t list
      -> unit -> t

  let create ctx ?name ?inst ?class_vars ?(declared_subs=[]) () = 
    let cvars = match class_vars with
      | None -> Variable.create StrMap.empty ctx
      | Some cv -> cv
    in
      Variable.create {
	class_name = name;
	class_instance = inst;
	class_parents = [];
	class_constants = StrMap.empty;
	class_methods = StrMap.empty;
	inst_vars = StrMap.empty;
	class_vars = cvars;
        declared_subtypes = declared_subs;
      } ctx

  let has_instance_class t = 
    match (Variable.deref t).class_instance with
      | None -> false | Some _ -> true

  let get_instance_class t = 
    let c = Variable.deref t in
      match c.class_instance with
        | Some x -> x
        | None -> 
            let ctx = Log.in_ctx (Variable.vctx t) "retrieving instance class" in
            let inst = KClass.create ctx () in
              c.class_instance <- Some inst;
              inst

  (* For a given class [t], return the class: t -> instance -> parent *)
  let over_up t = match (Variable.deref t).class_instance with
    | None -> (*Log.note "over:no instance";*)None
    | Some x -> match (Variable.deref x).class_parents with
	| [p] -> KTyp.as_class p
	| _ -> (*Log.note "over:wrong length list"; *)None

  (* Return the name of a class object.  If the name is not immedately
     known and the object has a single parent, the function
     recursively searches this parent.  This case can occur when an
     eigen class has been inserted into the hierarchy. *)
  let rec immediate_class_name cls_v = 
    let cls = Variable.deref cls_v in
      match cls.class_name with
	| (Some x) as s -> s
	| None -> match cls.class_parents with
	    | [x] -> 
		begin match KTyp.as_class x with 
		  | None -> None
		  | Some c -> immediate_class_name c
		end
	    | l -> None

  (* return the name of the class for a given instance.  This search
     begins with the instance class's parent (following Ruby's object
     model layout) *)
  let instance_class_name cls = 
    let par = over_up cls in
      match par with
	| None -> None
	| Some x -> immediate_class_name x

  let rec format_small ppf t = 
    match instance_class_name t, immediate_class_name t with
      | Some "Class", Some cname -> fmtf ppf "class %s" cname
      | Some iname, _ -> fmtf ppf "instance %s" iname
      | None, Some cname -> fmtf ppf "(meta?)-class %s" cname
      | None, None ->
          let meths = KClass.all_methods_respond t in
          let fields = (Variable.deref t).inst_vars in
          let name = default_opt "?" (instance_class_name t) in
            if StrMap.is_empty meths 
            then Format.pp_print_string ppf name
            else begin
              fmtf ppf "%s:[@[<h>" name;
              StrMap.iter (fun k v -> fmtf ppf "%s,@," k) meths;
              fmtf ppf "@]]"
            end;
            fmtf ppf "@[<h>%a@]"
              (format_strmap KField.format_small) fields

  let format_instance ppf o = match o with
    | None -> Format.pp_print_string ppf "?"
    | Some t -> KClass.format_small ppf t

  let format_strmap_nl format_f ppf map =
    let first = ref true in
    let format_inside ppf map = 
      StrMap.iter
	(fun k v ->
	   if !first then begin
	     first := false;
	     Format.fprintf ppf "%a" format_f v
	   end else Format.fprintf ppf "@\n%a" format_f v
	) map
    in
      Format.fprintf ppf "@[%a@]" format_inside map

  let inherit_count = ref 0
  let format_inherits ppf rents = 
    if conf.debug_level > 20 && !inherit_count < 10
    then begin
      incr inherit_count;
      List.iter (fun p -> KTyp.format ppf p) rents;
      decr inherit_count
    end else
      List.iter (fun p -> KTyp.format_small ppf p) rents

  let format_inst_parent ppf = function
    | None -> ()
    | Some i -> match (Variable.deref i).class_parents with
	| [] -> ()
	| p::_ -> KTyp.format_small ppf p

  let format_typ ppf t = 
    fmtf ppf ("@[<v>class %a@," ^^
		" instance of %a@," ^^
		" instance parent %a@," ^^
		" inherits from @[%a@]@," ^^
		" vars@," ^^
		"  @[%a@]@," ^^
		" class vars@," ^^
		"  @[%a@]@," ^^
		" methods@," ^^
		"  @[%a@]@," ^^
		" constants@," ^^
		"  @[%a@]@,@]")
      (format_option Format.pp_print_string) t.class_name
      format_instance t.class_instance
      format_inst_parent t.class_instance
      format_inherits t.class_parents
      (format_strmap_nl KField.format) t.inst_vars
      (format_strmap_nl KField.format) (Variable.deref t.class_vars)
      (format_strmap_nl KMethod.format) t.class_methods
      (format_strmap_nl (fun ppf f -> Format.pp_print_string ppf f.field_name))
      t.class_constants

  let format ppf t = 
    if conf.debug_level > 10 then fvar format_typ ppf t
    else format_small ppf t

  let unify_class_vars cgraph cmap1 cmap2 ~ctx =  
    let cvar_map = 
      strmap_union (Constraint.ret_unify KField.unify cgraph ~ctx)
        (Variable.deref cmap1) (Variable.deref cmap2)      
    in
    let cvars = Variable.create cvar_map ctx in
      if not (Variable.same cmap1 cvars)
      then CG.union_vars cgraph cmap1 cvars; 
      if not (Variable.same cmap2 cvars)
      then CG.union_vars cgraph cmap2 cvars

  let same_closed typ1 typ2 = 
    Variable.same typ1 typ2 ||
      match Variable.deref typ1, Variable.deref typ2 with
        | Typ_Closed p1, Typ_Closed p2 ->
            begin match KClass.immediate_class_name p1,
              KClass.immediate_class_name p2 with
                | Some n1, Some n2 -> n1 = n2
                | _ -> false
            end
        | _ -> false

  let unify cgraph cl1v cl2v ~ctx = 
    unify_var cgraph cl1v cl2v @< fun (cl1,cl2) ->
      (*Log.note ~ctx "unify class: %a %a" format_small cl1v format_small cl2v;*)
      let ctx1 = Variable.vctx cl1v in
      let ctx2 = Variable.vctx cl2v in
      let () = CG.union_vars cgraph cl2v cl1v in
      let () = match cl1.class_name, cl2.class_name with
	| None, None -> ()
	| None, Some x -> cl1.class_name <- cl2.class_name
	| Some x, None -> cl2.class_name <- cl1.class_name
	| Some x, Some y ->
	    if String.compare x y != 0 then
              Log.fatal ctx "@[<v 0>unifying classes with different names: %s %s@,%a@,%a@]"
                x y Log.format_ctx ctx1 Log.format_ctx ctx2
      in
      let vars = 
	strmap_union (Constraint.ret_unify KField.unify cgraph ~ctx)
	  cl1.inst_vars cl2.inst_vars 
      in
      let () = unify_class_vars cgraph cl1.class_vars cl2.class_vars ctx in
      let meths = 
	strmap_union (Constraint.ret_unify KMethod.unify cgraph ~ctx)
	  cl1.class_methods cl2.class_methods 
      in
      let consts = 
	strmap_union (Constraint.ret_unify KField.unify cgraph ~ctx)
	  cl1.class_constants cl2.class_constants 
      in
      let parents = match cl1.class_parents, cl2.class_parents with
	| [],[] -> []
	| _::_,[] -> cl1.class_parents
	| [],_::_ -> cl2.class_parents
	| _ ->  
            let uniq_cl1_parents = 
              List.fold_left
                (fun acc cl1_parent ->
                   if List.exists
                     (fun cl2_parent ->
                        if same_closed cl1_parent cl2_parent 
                        then (KTyp.unify cgraph cl1_parent cl2_parent ctx;true)
                        else false
                     ) cl2.class_parents
                   then acc
                   else cl1_parent::acc
                ) [] cl1.class_parents
            in
              (*
              if uniq_cl1_parents <> []
	      then Log.fixme ~ctx:(Log.merge ctx1 ctx2)
                "superclass mismatch?:@,@[<hov>%a@]@,@[<hov>%a@]@,@[<hov>%a@]"
                (format_comma_list KTyp.format) cl1.class_parents
                (format_comma_list KTyp.format) cl2.class_parents
                (format_comma_list Log.format_ctx) (List.map Variable.vctx uniq_cl1_parents)
              ;*)
              cl2.class_parents @ uniq_cl1_parents              
      in
      let inst = match cl1.class_instance, cl2.class_instance with
	| None, None -> None
	| None, Some x | Some x, None -> Some x
	| Some x, Some y -> 
	    (*CG.union_vars cgraph y x;*)
	    KClass.unify cgraph x y ctx; Some x
      in
      let declared_subtypes = match cl1.declared_subtypes,cl2.declared_subtypes with
        | [], [] -> []
        | [], lst | lst, [] -> lst
        | _ -> Log.fixme ~ctx "unify lists of declared subtypes"; cl2.declared_subtypes
      in
        cl1.class_name <- cl1.class_name;
	cl1.class_instance <- inst;
	cl1.class_parents <- parents;
	cl1.class_constants <- consts;
	cl1.class_methods <- meths;
	cl1.inst_vars <- vars;
	cl1.class_vars <- cl1.class_vars;
        cl1.declared_subtypes <- declared_subtypes;
        cl1v


  let union_parents ~get ~dupe_f c =
    (* the [seen] list is to avoid warnings from multiple inheritance
       where an ancestor is reachable by two+ paths.  List.memq should
       be fast since the length of this list (height of the hierarchy)
       should be short *)
    let rec work (cls:class_typ Variable.t) (acc,seen) = 
      if List.memq cls seen 
      then (acc,seen)
      else 
	let seen = cls::seen in
	let cls_ = Variable.deref cls in
	let acc = strmap_union dupe_f acc (get cls_) in
	  List.fold_left
	    (fun (acc,seen) x -> match Variable.deref x with
	       | Typ_Forall(_,p) | Typ_Closed p
	       | Typ_Open p -> work p (acc,seen)
	       | _ -> acc,seen
	    ) (acc,seen) cls_.class_parents
    in 
    let map, seen = work c (StrMap.empty,[]) in
      map
      
  let all_methods_inst cls_v = 
    let dupe_f m1 _ = m1 in
    let get cls = cls.class_methods in
      union_parents ~get ~dupe_f cls_v

  (** Compute the set of methods that an object instance will respond
      to.  This lookup procedure is slightly more general than Ruby's,
      but subsumes all of its behavior.  The procedure is as follows.
      Assume the following object layout:

           D
           |
      a -> a'  C
      |        |
      o -----> o'

      Arrows to the right represent class pointers and vertical bars
      show inheritance (subclass appearing below superclass).  First,
      the eigen methods of o and its parents are gathered.  That is,
      the methods contained in o' and a'.  Next, the instance methods
      of the classes for o and its parents are collected.  That is,
      the instance methods of o', C, 'a, D in that order (begignly
      searching a' and o' again to keep the code simpler).
  *)
  let all_methods_respond cls_v = 
    (* keep the first method we see when forming the union *)
    let dupe_f m1 _ = m1 in
    let rec work cls_v ((acc,seen) as orig) = 
      if List.memq cls_v seen 
      then orig
      else 
	let seen = cls_v::seen in
	let cls = Variable.deref cls_v in
        let acc = match (Variable.deref cls_v).class_instance with
          | None -> acc
	  | Some x -> strmap_union dupe_f acc (all_methods_inst x)
	in
	  List.fold_left
	    (fun (acc,seen) p -> match KTyp.as_class p with
	       | Some x -> work x (acc,seen)
	       | None -> acc,seen
	    ) (acc,seen) cls.class_parents
    in
    let get_eigen c = match c.class_instance with
      | None -> StrMap.empty
      | Some x -> (Variable.deref x).class_methods
    in
    let eigens = union_parents ~get:get_eigen ~dupe_f cls_v in
      fst (work cls_v (eigens,[]))
        
  let all_fields cls_v = 
    let targ = match (Variable.deref cls_v).class_instance with
      | None -> (*Log.fixme "all fields w/o instance?";*)cls_v
      | Some x -> x
    in
    let dupe_f f1 _ = (*Log.note "overloaded field %s" f1.field_name;*) f1 in
    let get cls = cls.inst_vars
      (*match cls.class_instance with
      | None -> StrMap.empty
      | Some x -> (Variable.deref x).inst_vars*)
    in
      union_parents ~get ~dupe_f targ

  let rec visit vtor t = 
    Visitor.visit vtor#visit_class t (visit_children vtor)

  and visit_children vtor classv = 
    let ctx = vtor#ctx in
    let clz = Variable.deref classv in
      (* don't support type variables appearring in parents... yet *)
    let inst = map_opt_preserve (visit vtor) clz.class_instance in
    let vars = map_preserve StrMap.map (KField.visit vtor) clz.inst_vars in
    let class_vars = Variable.deref clz.class_vars in
    let cvars = map_preserve StrMap.map (KField.visit vtor) class_vars in
    let meths = map_preserve StrMap.map (KMethod.visit vtor) clz.class_methods  in
    let consts = map_preserve StrMap.map (KField.visit vtor) clz.class_constants in
      if inst != clz.class_instance ||
	vars != clz.inst_vars ||
	cvars != class_vars ||
	meths != clz.class_methods ||
	consts != clz.class_constants
      then
	let class_vars' = Variable.create cvars ctx in
	  CG.union_vars vtor#cgraph clz.class_vars class_vars';
	  vcreate "class"
	    {class_name = clz.class_name;
	     class_parents = clz.class_parents; 
	     class_instance = inst;
	     inst_vars = vars;
	     class_vars = clz.class_vars; (* FIXME *)
	     class_methods = meths;
	     class_constants = consts;
             declared_subtypes = [];
	    } ctx
      else classv

  let rec add_parent cg ~parent cls_v ~ctx = 
    let cls = Variable.deref cls_v in
      cls.class_parents <- parent::cls.class_parents;
      CG.requeue cg (Variable.vid cls_v)
	  
  let add_method cg mt cls_v ctx = 
    (*Log.fixme "adding method %s to [%d]%a" mt.method_name 
      (Variable.vid cls_v) KClass.format cls_v;*)
    let cls = Variable.deref cls_v in
    let name = mt.method_name in
    let () = (* unify with old method *)
      try let old_mt = StrMap.find name cls.class_methods in
        (*Log.fixme "unify %s %s\n" old_mt.method_name mt.method_name;*)
        KMethod.unify cg old_mt mt ~ctx
      with Not_found -> () 
    in
    let meths = StrMap.add name mt cls.class_methods in
      cls.class_methods <- meths;
      CG.requeue cg (Variable.vid cls_v)

  let add_constant cg name t cls_v ctx = 
    let cls = Variable.deref cls_v in
    let fld = KField.create ctx name ~t () in
      if StrMap.mem name cls.class_constants
      then Log.fatal ctx "trying to add existing constant %s to class" name;
      cls.class_constants <- StrMap.add name fld cls.class_constants;
      CG.requeue cg (Variable.vid cls_v)

  let lookup_constant cg name cls_v ctx = 
    (*Log.note "looking up constant %s" name;*)
    let cls = Variable.deref cls_v in
      try (StrMap.find name cls.class_constants).field_typ
      with Not_found -> 
	(*Log.note "didn't find constant %s" name;*)
	let fld = KField.create ctx name () in
	  cls.class_constants <- StrMap.add name fld cls.class_constants;
	  CG.requeue cg (Variable.vid cls_v);
	  fld.field_typ

  let mem_constant name cls_v = 
    StrMap.mem name (Variable.deref cls_v).class_constants

  let rec lookup_class_var cg name cls_v ctx = 
    let cls_v = get_instance_class cls_v in
    let cls = Variable.deref cls_v in
    let map = Variable.deref cls.class_vars in
      try (StrMap.find name map).field_typ
      with Not_found ->
	let fld = KField.create ctx name () in
	let map' = StrMap.add name fld map in
	  CG.union_vars cg cls.class_vars (Variable.create map' ctx);
	  CG.requeue cg (Variable.vid cls_v);
	  fld.field_typ

  let lookup_class_inst_var cg name cls_v ctx = 
    let cls = Variable.deref cls_v in
      try (StrMap.find name cls.inst_vars).field_typ
      with Not_found ->
	let fld = KField.create ctx name () in
	  cls.inst_vars <- StrMap.add name fld cls.inst_vars;
	  CG.requeue cg (Variable.vid cls_v);
	  fld.field_typ

  let lookup_inst_var cg name cls_v ctx = 
    let cls_v = get_instance_class cls_v in
      lookup_class_inst_var cg name cls_v ctx

  let alias_method cg ~exists ~link cls_v t ctx = 
    let cls = Variable.deref cls_v in
    try
      let old_mt = StrMap.find exists (all_methods_inst cls_v) in
      let new_mt = {old_mt with method_name=link} in
	cls.class_methods <- StrMap.add link new_mt cls.class_methods;
	CG.requeue cg (Variable.vid cls_v)
    with Not_found ->
      Log.err ~ctx "when aliasing %s to %s, didn't find original method @\n in %a"
	exists link KClass.format cls_v;
      let new_mt = KMethod.create ctx link () in
	cls.class_methods <- StrMap.add link new_mt cls.class_methods;
	CG.requeue cg (Variable.vid cls_v)

  let module_function cg mname cls_v ctx = 
    let cls = Variable.deref cls_v in
    let targ_cls_v = match cls.class_instance with
      | None -> Log.fatal ctx "can't find class instance for module_function"
      | Some x -> x
    in
    let targ_cls = Variable.deref targ_cls_v in      
    let mt = 
      try StrMap.find mname (all_methods_inst cls_v) 
      with Not_found ->
        Log.err ~ctx "when copying %s for module_function, didn't find original method @\n in %a"
	  mname KClass.format cls_v;
        KMethod.create ctx mname ()
    in
	targ_cls.class_methods <- StrMap.add mname mt targ_cls.class_methods;
	CG.requeue cg (Variable.vid targ_cls_v)

end and KField : Kind_S
  with type t=field_typ 
  and type create_sig = string -> ?t:KTyp.t -> unit -> KField.t
  = 
struct

  type t = field_typ
  type create_sig = string -> ?t:KTyp.t -> unit -> KField.t
  let create ctx name ?t () =
    { field_name = name;
      field_typ = default_opt (KTyp.create ctx) t;
    }

  let format_small ppf t = 
    fmtf ppf "{%s:%a}" t.field_name 
      KTyp.format_small t.field_typ

  let format ppf t = 
    fmtf ppf "{%s:%a}" t.field_name 
      KTyp.format t.field_typ

  let unify cgraph f1 f2 ~ctx = 
    if String.compare f1.field_name f2.field_name != 0
    then Log.fatal ctx 
      "application error: fields with different names %s <> %s ?????"
      f1.field_name f2.field_name
    else KTyp.unify cgraph f1.field_typ f2.field_typ ~ctx

  let rec visit vtor t = 
    Visitor.visit vtor#visit_field t (visit_children vtor)
  and visit_children vtor ft = 
    let ft' = KTyp.visit vtor ft.field_typ in
      if ft.field_typ != ft'
      then {field_name = ft.field_name; field_typ = ft' }
      else ft

end and KMethod : 
sig
  include Kind_S with type t=method_typ
		 and type create_sig = string -> ?func:func_sig -> ?block:block_typ Variable.t
		   -> unit -> method_typ
  val method_name : t -> string
  val unify_method_kind : CG.t -> method_kind Variable.t -> method_kind Variable.t
    -> ctx:Log.ctx -> unit

  val instantiate_annotation : CG.t -> t -> Log.ctx -> t option

end = struct
  type t = method_typ
  type create_sig = string -> ?func:KFunc.t -> ?block:KBlock.t -> unit -> KMethod.t

  let create ctx name ?func ?block () = 
    let blk = match block with
      | None -> KBlock.create ctx ()
      | Some x -> x
    in match func with
      | Some sg ->
	  {method_name = name;
	   method_kind = Variable.create (MonoMethod(sg,blk)) ctx;
	  }
      | None -> {method_name=name;method_kind=Variable.create MethodVar ctx}

  let method_name mt = mt.method_name

  let format_small ppf t = fmtf ppf "{meth:%s}" t.method_name

  let format_sig_blk ppf (sg,blk) = 
    fmtf ppf "@[%a {%a}@]"
      KFunc.format sg
      KBlock.format blk

  let rec format_kind name ppf kind = 
    match Variable.deref kind with
      | MethodVar -> fmtf ppf "[%s : ?]" name
	  
      | MonoMethod(sig',blk) ->
	  fmtf ppf "@[%s : %a (%a)@]" 
	    name KFunc.format sig' KBlock.format blk
	    
      | ForallMethod(gamma,mt') ->
	  fmtf ppf "@[%s : %a . @[%a@]@]"
	    name
            (format_comma_list
               (fun ppf (t1,t2o) -> match t2o with
                  | None ->
                      fmtf ppf "(%d => (%a <: T))" (Variable.vid t1)
                        KTyp.format_small t1
                  | Some t2 ->
                      fmtf ppf "(%d => (%a <: %a))" (Variable.vid t1)
                        KTyp.format_small t1 KTyp.format_small t2

               )
            ) (Variable.deref gamma)
	    (format_kind name) mt'
	    
      | InterMethod(lst) ->
	  fmtf ppf "@[%s : @[%a@]@]"
	    name (format_break_list format_sig_blk) lst

  let rec format ppf mt = format_kind mt.method_name ppf mt.method_kind

  let unify_sg_blk cgraph (sg1,blk1) (sg2,blk2) ~ctx = 
    KFunc.unify cgraph sg1 sg2 ~ctx;
    KBlock.unify cgraph blk1 blk2 ~ctx

  let rec unify_method_kind cgraph m1u m2u ~ctx : unit = 
    unify_var cgraph m1u m2u @< function
      | MonoMethod(sig1,blk1), MonoMethod(sig2,blk2) ->
          CG.union_vars cgraph m2u m1u;
	  KFunc.unify cgraph sig1 sig2 ~ctx;
	  KBlock.unify cgraph blk1 blk2 ~ctx;
	  m1u

      | ForallMethod(gamma1,sub1), ForallMethod(gamma2,sub2) ->
          CG.union_vars cgraph m2u m1u;
          KTyp.unify_poly_env cgraph gamma1 gamma2 ctx;
          unify_method_kind cgraph sub1 sub2 ctx;
          m1u

      | InterMethod(lst1), InterMethod(lst2) ->
          CG.union_vars cgraph m2u m1u;
	  begin try
	    List.iter2 (unify_sg_blk cgraph ~ctx) lst1 lst2;
	    m1u
	  with Invalid_argument _ ->
	    Log.err "unify polymethod: different size intersections";
	    m1u
	  end

      | MethodVar, m2 -> m2u
      | m1, MethodVar -> m1u

      | MonoMethod _, ForallMethod _  ->
	  Log.err ~ctx "unifying mono and polymorphic methods";
	  m2u
      | ForallMethod _, MonoMethod _ ->
	  Log.err ~ctx "unifying mono and polymorphic methods";
	  m1u
	  
      | InterMethod _ , MonoMethod _ -> 
	  Log.err ~ctx "unifying mono and intersection methods";
	  m1u
      | MonoMethod _, InterMethod _ ->
	  Log.err ~ctx "unifying mono and intersection methods";
	  m2u

      | ForallMethod _, InterMethod _ 
      | InterMethod _, ForallMethod _ ->
	  Log.fatal ctx "unifying poly and intersection methods"

  let unify cgraph m1 m2 ~ctx : unit = 
    (*Log.note "unify meth: %a <=> %a" format_small m1 format_small m2;*)
    (*if (String.compare m1.method_name m2.method_name) != 0
    then Log.fatal ctx
      "unify_method should only be called with methods of the same name %s <> %s"
      m1.method_name m2.method_name;*)
    let ctx = Log.in_ctx ctx "unifying methods %s %s" 
      m1.method_name m2.method_name 
    in
      unify_err unify_method_kind cgraph m1.method_kind m2.method_kind ~ctx

  let rec visit vtor t = 
    Visitor.visit vtor#visit_method t (visit_children vtor)

  and visit_children vtor mt = 
    let kind' = visit_meth_kind vtor mt.method_kind in
      if mt.method_kind != kind' 
      then {mt with method_kind=kind'}
      else mt

  and visit_meth_kind vtor kind = match Variable.deref kind with
    | MethodVar -> kind
	  
    | MonoMethod(func,blk) -> 
	let func' = KFunc.visit vtor func in
	let blk' = KBlock.visit (vtor#flip_variance) blk in (*contra*)
	  if func != func' || blk != blk'
	  then vcreate "monometh" (MonoMethod(func', blk')) vtor#ctx
	  else kind
	    
    | InterMethod([]) -> assert false
    | InterMethod(ms) ->
	let ms' = map_preserve List.map (visit_sg_blk vtor) ms in
	  if ms != ms'
	  then vcreate "intermeth" (InterMethod(ms')) vtor#ctx
	  else kind
	    
    | ForallMethod(fenv,subm) ->
        let fenv' = KTyp.visit_poly_env vtor fenv in
	let subm' = visit_meth_kind vtor subm in
	  if subm != subm' || fenv != fenv'
	  then vcreate "forallmeth" (ForallMethod(fenv',subm')) vtor#ctx
	  else kind
  
  and visit_sg_blk vtor ((sg,blk) as orig) = 
    let sg' = KFunc.visit vtor sg in
    let blk' = KBlock.visit (vtor#flip_variance) blk in (*contra*)
      if sg != sg' || blk != blk'
      then (sg',blk') else orig

  let instantiate_annotation cg mt ctx = match Variable.deref mt.method_kind with
    | MonoMethod _ -> Some mt
    | ForallMethod(fenv,mk) -> 
        begin match Variable.deref mk with
          | InterMethod _ -> 
              Log.warn "cant check intersection types yet";
              None
          | _ ->
              let env = Variable.deref fenv in
              let mt' = KSubstitution.method_witness cg env mt.method_name mk ctx in
                Some mt'
        end
    | InterMethod _ -> 
        Log.warn "cant check intersection types yet";
        None
    | MethodVar -> 
        Log.fatal Log.empty "Annotation was MethodVar?"

end and KParam : 
sig
  type params = KTyp.t param_typ_
  include Kind_S with type t=t param_typ_ Variable.t 
		 and type create_sig = param_typ Variable.t 

  type any_list = [
  | `Param_Default of KTyp.t * any_list
  | `Param_Star of KTyp.t
  | `Param_Empty
  | `Param_Var
  | `Param_t of KTyp.t * any_list
  | `Param_tuple of KTyp.t * KTyp.t * any_list
  ]

end = 
struct
  
  type params = KTyp.t param_typ_

  and t = params Variable.t
  type create_sig = t
  let create ctx = Variable.create `Param_Var ctx

  type any_list = [
  | `Param_Default of KTyp.t * any_list
  | `Param_Star of KTyp.t
  | `Param_Empty
  | `Param_Var
  | `Param_t of KTyp.t * any_list
  | `Param_tuple of KTyp.t * KTyp.t * any_list
  ]

  let __test (pl : param_typ) (dl : default_list) = 
    ignore(pl : param_typ :> any_list);
    ignore(dl : default_list :> any_list)


  let format_small ppf pl = fmtf ppf "<params>"

  let rec format_any_params ppf (pl : any_list) = match pl with
    | `Param_Var -> fmtf ppf "?"
    | `Param_tuple(tup,elt,ts) -> 
	fmtf ppf "(%a), %a" KTyp.format_small tup format_any_params ts
    | `Param_t(t,ts) -> 
	fmtf ppf "%a, %a" KTyp.format_small t format_any_params ts
    | `Param_Default(t,ts) -> 
	fmtf ppf "?%a, %a" KTyp.format_small t format_any_params ts
    | `Param_Star t -> fmtf ppf "*%a" KTyp.format_small t
    | `Param_Empty -> ()
	
  let format_typ ppf pl = 
    format_any_params ppf (pl : param_typ :> any_list)

  let format ppf t = fvar format_typ ppf t

  let rec convert_params_to_default : params -> default_list = function
    | #default_list as d -> d
    | `Param_Var -> Log.fatal Log.empty "trying to convert var to default list??"
    | `Param_tuple(tup,elt,rest) -> 
	Log.fatal Log.empty "trying to convert tuple to default list??"
    | `Param_t(t,rest) ->
	let rest' = convert_params_to_default rest in
	  `Param_Default(t,rest')
	    
  let rec unify_params_exn cgraph p1 p2 ~ctx = 
    let rec work (pl1: params) (pl2 : params) : params = 
      match pl1,pl2 with
	| `Param_Var, _ -> pl2
	| _, `Param_Var -> pl1

	| `Param_Empty, `Param_t _
	| `Param_t _, `Param_Empty ->
	    let msg = sprintf "functions with different arities" in
	      raise (Unify_Error(ctx,msg))

	| (`Param_Default _ as d), `Param_Empty
	| `Param_Empty, (`Param_Default _ as d) ->
	    (d :> params)

	| `Param_t(t1,rest1), `Param_t(t2,rest2) ->
	    let rest = work rest1 rest2 in
	      KTyp.unify cgraph t1 t2 ~ctx;
	      `Param_t(t1, rest)
		
	| `Param_Default(t1,rest1), `Param_t(t2,rest2) ->
	    let rest2 = convert_params_to_default rest2 in 
	    let rest = work_default rest1 rest2 in
	      KTyp.unify cgraph t1 t2 ctx;
	      `Param_Default(t1, rest)

	| `Param_t(t1,rest1), `Param_Default(t2,rest2) ->
	    let rest1 = convert_params_to_default rest1 in 
	    let rest = work_default rest1 rest2 in
	      KTyp.unify cgraph t1 t2 ctx;
	      `Param_Default(t1, rest)

	| `Param_tuple(t1,elt1,rest1), `Param_tuple(t2,elt2,rest2) ->
	    let rest = work rest1 rest2 in
	      KTyp.unify cgraph t1 t2 ~ctx;
	      KTyp.unify cgraph elt1 elt2 ~ctx;
	      `Param_tuple(t1, elt1, rest)

	| `Param_tuple _, _ | _, `Param_tuple _ ->
	    Log.err ~ctx "mixing tuple and other arg in unify?";
	    pl1

	| `Param_Star _, `Param_t _
	| `Param_t _, `Param_Star _ ->
	    Log.err ~ctx "mixing star and nominal arg in unify?";
	    pl1

	| (#default_list as d1), (#default_list as d2) ->
	    let x = work_default d1 d2 in
	      (x :> params)

    and work_default (d1:default_list) (d2:default_list) : default_list = 
      match d1,d2 with
	| `Param_Star t1, `Param_Star t2 -> 
	    KTyp.unify cgraph t1 t2 ctx;
	    `Param_Star t1
	| `Param_Empty, `Param_Empty -> `Param_Empty

	| `Param_Star _, (`Param_Empty as e)
	| (`Param_Empty as e), `Param_Star _ ->
	    e
	    
	| `Param_Default(t1,rest1), `Param_Default(t2,rest2) ->
	    let rest = work_default rest1 rest2 in
	      KTyp.unify cgraph t1 t2 ~ctx;
	      `Param_Default(t1,rest)
		
	| (`Param_Default _ as d), (`Param_Empty | `Param_Star _) 
	| (`Param_Empty | `Param_Star _), (`Param_Default _ as d) ->
	    Log.err ~ctx "mixing default and empty/star arg in unify?";
	    d
    in
      unify_var cgraph p1 p2
	(fun (pl1,pl2) ->
	   let p' = work pl1 pl2 in
	     if p' == pl1 then p1
	     else if p' == pl2 then p2
	     else Variable.create p' ctx
	)

  let unify cgraph p1 p2 ~ctx = 
    (*Log.note "unify param: %a <=> %a" format_small p1 format_small p2;*)
    unify_err unify_params_exn cgraph p1 p2 ~ctx

  let rec visit vtor t = 
    Visitor.visit vtor#visit_param t (visit_children vtor)

  and visit_children vtor params = 
    let p = Variable.deref params in
    let p' = visit_param vtor p in
      if p != p' then vcreate "params" p' vtor#ctx
      else params
            
  and visit_param vtor (pl : param_typ) : param_typ = match pl with
    | `Param_Var -> pl
    | `Param_t(t,ts) -> 
	let t' = KTyp.visit vtor t in
	let ts' = visit_param vtor ts in
	  if t != t' || ts != ts'
	  then `Param_t(t', ts')
	  else pl
            
    | `Param_tuple(t,elt,ts) -> 
	let t' = KTyp.visit vtor t in
	let elt' = KTyp.visit vtor elt in
	let ts' = visit_param vtor ts in
	  if t != t' || elt != elt' || ts != ts'
	  then `Param_tuple(t', elt', ts')
	  else pl
            
    | #default_list as d -> (visit_def vtor d :> param_typ)
        
  and visit_def vtor (dl:default_list) : default_list = match dl with
    | `Param_Default(t,ts) -> 
	let t' = KTyp.visit vtor t in
	let ts' = visit_def vtor ts in
	  if t != t' || ts != ts'
	  then `Param_Default(t', ts')
	  else dl
      | `Param_Star t ->
	  let t' = KTyp.visit vtor t in
	    if t != t'
	    then `Param_Star t'
	    else dl
      | `Param_Empty -> dl
    
end and KBlock : 
sig
  include Kind_S with type t=block_typ Variable.t
		 and type create_sig = ?bsig:func_sig -> unit -> block_typ Variable.t

  val no_block : Log.ctx -> KBlock.t
  val to_func : CG.t -> KBlock.t -> Log.ctx -> KFunc.t option

end = 
struct
  type t = block_typ Variable.t
  type create_sig = ?bsig:KFunc.t -> unit -> KBlock.t

  let create ctx ?bsig () = match bsig with
    | None -> Variable.create Block_Var ctx
    | Some sig' -> Variable.create (Block sig') ctx

  let no_block ctx = Variable.create Block_None ctx

  let to_func cg blk ctx = match Variable.deref blk with
    | Block_None -> None
    | Block_Var -> 
	let sg = KFunc.create ctx () in
	let blk' = Variable.create (Block sg) ctx in
	  (CG.union_vars cg blk blk');
	  Some sg
    | Block sg -> Some sg

  let format_small ppf t = match Variable.deref t with
    | Block_None -> fmtf ppf "NoBlock"
    | Block_Var -> fmtf ppf "BlkVar"
    | Block s -> fmtf ppf "Blk(%a)" KFunc.format_small s

  let format ppf t = match Variable.deref t with
    | Block_None -> fmtf ppf "NoBlock"
    | Block_Var -> fmtf ppf "BlkVar"
    | Block s -> fmtf ppf "Blk(%a)" KFunc.format s

  let rec unify_exn cgraph b1 b2 ~ctx = 
    unify_var cgraph b1 b2 @< function
      | Block_Var, _ -> b2
      | _, Block_Var -> b1
      | Block_None, Block_None -> b1
      | Block_None, Block _ ->
	  (*if conf.strict_block_check
	  then raise(Unify_Error(ctx, "block none <> block some"))
	  else*) b2
      | Block _, Block_None ->
	  (*if conf.strict_block_check
	  then raise(Unify_Error(ctx, "block none <> block some"))
	  else*) b1
      | Block blk1, Block blk2 ->
          CG.union_vars cgraph b2 b1;
	  KFunc.unify cgraph blk1 blk2 ~ctx;
	  b1

  let unify cgraph b1 b2 ~ctx = 
    (*Log.note "unify block: %a <=> %a" format_small b1 format_small b2;*)
    unify_err unify_exn cgraph b1 b2 ~ctx
	
  let rec visit vtor t = 
    Visitor.visit vtor#visit_block t (visit_children vtor)

  and visit_children vtor blk = match Variable.deref blk with
    | Block_None -> blk
    | Block_Var -> blk
    | Block s ->
	let s' = KFunc.visit vtor s in
	  if s != s' 
	  then vcreate "blk" (Block s') vtor#ctx
	  else blk
	    
end and KFunc : 
sig
  include Kind_S with type t=func_sig
		 and type create_sig = ?self:t -> ?args:param_typ Variable.t
                     -> ?ret:t -> unit -> func_sig

  val get_self : t -> KTyp.t
  val get_params : t -> KParam.t
  val get_ret : t -> KTyp.t

end = 
struct
  type t = func_sig

  type create_sig = ?self:KTyp.t -> ?args:KParam.t -> ?ret:KTyp.t -> unit -> KFunc.t
  let create ctx ?self ?args ?ret () = 
    {sig_self = default_opt (KTyp.create ctx) self;
     sig_args = default_opt (Variable.create `Param_Var ctx) args;
     sig_ret = default_opt (KTyp.create ctx) ret;
    }
      
  let get_self t = t.sig_self
  let get_params t = t.sig_args
  let get_ret t = t.sig_ret

  let format_small ppf s = fmtf ppf "(...) -> ."

  let format ppf s =
    fmtf ppf "(@[self: %a, @[%a@]@]) -> @[%a@]"
      KTyp.format_small s.sig_self
      KParam.format s.sig_args
      KTyp.format_small s.sig_ret

  let unify cgraph s1 s2 ~ctx = 
    KTyp.unify cgraph s1.sig_self s2.sig_self ~ctx;
    KParam.unify cgraph s1.sig_args s2.sig_args ~ctx;
    KTyp.unify cgraph s1.sig_ret s2.sig_ret ~ctx

  let rec visit vtor t = 
    Visitor.visit vtor#visit_func t (visit_children vtor)

  and visit_children vtor sg = 
    let self' = KTyp.visit (vtor#flip_variance) sg.sig_self in (*contra*)
    let args' = KParam.visit (vtor#flip_variance) sg.sig_args in (*Contra*)
    let ret' = KTyp.visit vtor sg.sig_ret in
      if sg.sig_self != self' 
	|| sg.sig_args != args' 
	|| sg.sig_ret != ret' 
      then {sig_self = self'; sig_args = args'; sig_ret = ret'} 
      else sg

end and KSubstitution : 
sig
    
  val instantiate_closed : CG.t -> poly_env Variable.t ->
    KClass.t -> ?params:KTyp.t list -> Log.ctx -> KClass.t

  val instantiate_polymethod : CG.t -> poly_env Variable.t -> 
    string -> method_kind Variable.t -> Log.ctx -> KMethod.t

  val method_witness : CG.t -> poly_env ->
    string -> method_kind Variable.t -> Log.ctx -> KMethod.t

  val class_witness : CG.t -> poly_env -> KClass.t -> Log.ctx -> KClass.t

  val instantiate_combined_witness : CG.t -> string ->
    (poly_env Variable.t * method_kind Variable.t) ->
    (poly_env Variable.t * method_kind Variable.t) ->
    Log.ctx -> (KMethod.t * KMethod.t)

end = struct

  module TypList = struct
    type t = KTyp.t list
    let rec compare x y = match x,y with
      | [],[] -> 0
      | _::_, [] ->  1
      | [], _::_ -> -1
      | hx::xs, hy::ys -> 
	  match Pervasives.compare (Variable.vid hx) (Variable.vid hy) with
	    | 0 -> compare xs ys
	    | c -> c

  end
  module TypListMap = Map.Make(TypList)

  let var_id t = match Variable.deref t with
    | Typ_PVar -> Variable.vid t
    | Typ_Param p -> Variable.vid p
    | _ -> 
        Log.fatal (Variable.vctx t) "polymorphic id has been unified"

  let add_params_to_inst_tbl cg tbl (fenv:poly_env) params ctx =
    try List.iter2 
      (fun (t1,bound_opt) p -> 
         begin match bound_opt with
         | None -> ()
         | Some bound ->
             ConTyp.add_constraint cg p bound ~ctx
               ~origin:(Constraint.Source ctx)
         end;
         Hashtbl.add tbl (Variable.vid t1) p;
         Hashtbl.replace tbl (var_id t1) p
      ) fenv params
    with Invalid_argument _ ->
      Log.fatal ctx
        "wrong number of arguments to instantiate forall, got %d, expected %d"
        (List.length params) (List.length fenv)
          
  class instantiation_visitor cg_ ctx_ env_ = 
  object(self)
    inherit kind_visitor cg_ ctx_ as super
    val env = env_
    val inst_tbl = Hashtbl.create 127

    method private up_cast = (self :> kind_visitor)

    method visit_typ t = match Variable.deref t with
      | Typ_PVar ->
	  begin try Visitor.ChangeTo (Hashtbl.find env (Variable.vid t))
	  with Not_found -> Visitor.SkipChildren
	  end

      | Typ_Param p -> begin match Variable.deref p with
	  | `Param_Var -> 
	      begin try
	        let t' = Hashtbl.find env (Variable.vid p) in
		  match Variable.deref t' with
		    | Typ_Param _ -> 
                        Visitor.ChangeTo t'
		    | _ -> 
                        Log.fatal ctx
                          "annotation error: variable should have kind param, got %a"
                          KTyp.format t'
	      with Not_found -> Visitor.SkipChildren
	      end
	  | p -> super#visit_typ t
        end
            
      | Typ_Inst(p,None,tr) -> Visitor.SkipChildren
          
      | Typ_Inst(body,Some args,tr) ->
	  let args' = map_preserve List.map (KTyp.visit self#up_cast) args in
	    if args == args'
	    then Visitor.SkipChildren
	    else if List.for_all (fun x -> Hashtbl.mem env (Variable.vid x)) args
	    then begin
	      let tid = Variable.vid body in
	      let f ctx = 
	        match (Variable.deref body) with
		  | Typ_Forall(fenvu,clz) ->
		      let env' = Hashtbl.copy env in
                      let fenv = Variable.deref fenvu in
		      let () = add_params_to_inst_tbl self#cgraph env' fenv args' ctx in
                      let dup = {<env=env'>} in
		      let ec' = KClass.visit (dup :> kind_visitor) clz in
		      let close = KTyp.of_closed ec' ctx in
		        KTyp.new_instance self#cgraph close ctx
		  | _ -> t
	      in
                Visitor.ChangeTo(self#update_tlist_map tid args' f)
	    end else 
              Visitor.ChangeTo(vcreate "t inst" (Typ_Inst(body,Some args',tr)) self#ctx)

      | _ -> Visitor.DoChildren
          
    method private update_tlist_map type_id arg_list f = 
      try let map = Hashtbl.find inst_tbl type_id in
	try TypListMap.find arg_list map 
	with Not_found -> 
	  let fresh_t = KTyp.create ctx in
	  let map' = TypListMap.add arg_list fresh_t map in
	  let () = Hashtbl.replace inst_tbl type_id map' in
	  let t' = f ctx in
	    KTyp.unify self#cgraph fresh_t t' ctx;
	    t'
      with Not_found -> 
        let fresh_t = KTyp.create ctx in
        let map = TypListMap.add arg_list fresh_t TypListMap.empty in
	  Hashtbl.add inst_tbl type_id map;
	  let t' = f ctx in
	    KTyp.unify self#cgraph fresh_t t' ctx;
	    Hashtbl.replace inst_tbl (Variable.vid t') map;
	    t'    
              
    method visit_param params = 
      match Variable.deref params with
	| `Param_Var -> 
	    begin try
	      let t' = Hashtbl.find env (Variable.vid params) in
		match Variable.deref t' with
		  | Typ_Param p -> Visitor.ChangeTo p
		  | _ -> 
                      Log.fatal ctx
                        "annotation error: variable should have kind param, got %a"
                        KTyp.format t'
	    with Not_found -> Visitor.SkipChildren
	    end
	| p -> super#visit_param params

  end

  let instantiate_qvar t ctx = match Variable.deref t with
    | Typ_PVar -> Variable.vid t, KTyp.create ctx
    | Typ_Param p -> Variable.vid p, KTyp.of_param (KParam.create ctx) ctx
    | _ -> 
        Log.fatal ctx "polymorphic var has been unified with a structure?"

  let add_bound cg t1 bound_opt ctx = match bound_opt with
    | None -> ()
    | Some bound ->
        ConTyp.add_constraint cg t1 bound ~ctx
          ~origin:(Constraint.Source ctx)

  let instantiate_closed cg fenv_u clz ?params ctx = 
    let fenv = Variable.deref fenv_u in
    let inst_tbl = Hashtbl.create 127 in
    let () = match params with
      | None -> 
          List.iter
            (function t1,bound -> 
               let i,t = instantiate_qvar t1 ctx in
                 Hashtbl.add inst_tbl i t;
                 Hashtbl.replace inst_tbl (Variable.vid t1) t;
                 add_bound cg t bound ctx
            ) fenv
      | Some lst -> 
          assert((List.length lst) = (List.length fenv));
          List.iter2
            (fun (t1,bound) t -> 
               Hashtbl.add inst_tbl (var_id t1) t;
               Hashtbl.replace inst_tbl (Variable.vid t1) t;               
               add_bound cg t bound ctx
            ) fenv lst
    in
    let vtor = new instantiation_visitor cg ctx inst_tbl in
      KClass.visit (vtor:>kind_visitor) clz
        
  let instantiate_polymethod cg (env:poly_env Variable.t) mname mt ctx = 
    let env_tbl = Hashtbl.create 127 in
    let () = List.iter
      (fun (t1,bound) -> 
         let i', t' = instantiate_qvar t1 ctx in
           (* map both the type kind id and the param kind id *)
           Hashtbl.add env_tbl (Variable.vid t1) t';
           Hashtbl.replace env_tbl i' t';
           add_bound cg t' bound ctx
      ) (Variable.deref env)
    in
    let m = {method_name = mname;method_kind = mt} in
    let vtor = new instantiation_visitor cg ctx env_tbl in
      let m' = KMethod.visit (vtor :> kind_visitor) m in
        if conf.debug_constraints
        then Log.note "@[<v>polymethod inst:@, %a@, to@,%a" 
          KMethod.format m KMethod.format m';
        m'

  let witness_type t bound ctx = 
    match bound with
      | Some s -> Variable.create (Typ_Witness(Variable.vid t,s)) ctx
      | None -> match Variable.deref t with
          | Typ_Param _ -> 
              let wit = Variable.create (Typ_Witness((Variable.vid t),top)) ctx in
                KTyp.of_param (Variable.create (`Param_Star wit) ctx) ctx
          | _ -> 
              Variable.create (Typ_Witness((Variable.vid t),top)) ctx

  let witness_visitor cg env ctx = 
    let env_tbl = Hashtbl.create 127 in
    let () = List.iter
      (fun (t1,bound) -> 
         let i' = var_id t1 in
         let witness = witness_type t1 bound ctx in
           (* map both the type kind id and the param kind id *)
           Hashtbl.add env_tbl (Variable.vid t1) witness;
           Hashtbl.replace env_tbl i' witness;
      ) env
    in
      new instantiation_visitor cg ctx env_tbl

  let method_witness cg env mname mt ctx = 
    let vtor = witness_visitor cg env ctx in
    let m = {method_name = mname;method_kind = mt} in
      KMethod.visit (vtor :> kind_visitor) m

  let class_witness cg env cls ctx = 
    let vtor = witness_visitor cg env ctx in
      KClass.visit (vtor :> kind_visitor) cls

  let instantiate_combined_witness cg mname (env1,mt1) (env2,mt2) ctx = 
    let env_tbl = Hashtbl.create 127 in
    let rec comb_iter list1 list2 = match list1,list2 with
      | [], [] -> ()
      | [], y::ys -> 
          Log.err ~ctx "rhs polymethod has more quantifiers"; 
          ()

      | (t1,b1)::xs, (t2,b2)::ys -> 
          let bound = KTyp.unify_bound cg b1 b2 ctx in
          let i1 = var_id t1 in
          let i2 = var_id t2 in
          let witness = witness_type t1 bound ctx in
            (* map both the type kind id and the param kind id *)
            Hashtbl.replace env_tbl (Variable.vid t1) witness;
            Hashtbl.replace env_tbl i1 witness;
            Hashtbl.replace env_tbl (Variable.vid t2) witness;
            Hashtbl.replace env_tbl i2 witness;
            comb_iter xs ys

      | (t,bound)::tl, [] -> 
         let i = var_id t in
         let witness = witness_type t bound ctx in
           (* map both the type kind id and the param kind id *)
           Hashtbl.add env_tbl (Variable.vid t) witness;
           Hashtbl.replace env_tbl i witness;
           comb_iter tl []
    in
    let () = comb_iter (Variable.deref env1) (Variable.deref env2) in
    let m1 = {method_name = mname;method_kind = mt1} in
    let m2 = {method_name = mname;method_kind = mt2} in
    let vtor = new instantiation_visitor cg ctx env_tbl in
    let m1' = KMethod.visit (vtor :> kind_visitor) m1 in
    let m2' = KMethod.visit (vtor :> kind_visitor) m2 in
      m1', m2'

end and ErrorTrace : sig
    val no_method_error : CG.t -> KClass.t -> KClass.t -> string list -> unit
end = struct

    module Label = struct
      type t = (int * Log.ctx) option
      let default = None
      let compare = compare
    end

    module G = Graph.Imperative.Digraph.ConcreteLabeled(Utils.Int)(Label)

    module Printable = struct
      include G

      let start = ref None
      let fin = ref None
        
      let edge_attributes (_,l,_) = match l with
        | Some _ -> []
        | None -> [`Color 0xFF0000]
      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_name v = string_of_int v
      let vertex_attributes v = 
        if Some v = !start || Some v = !fin
        then [`Color 0x00FF00]
        else []
      let default_vertex_attributes _ = []
      let graph_attributes _ = [`Size(14.,14.)]
    end
    module Dot = Graph.Graphviz.Dot(Printable)

    module W = struct
      type t = int
      type label = Label.t
      let weight _ = 1
      let zero = 0
      let add = (+)
      let compare (x:int) (y:int) = compare x y
    end

    module Dijkstra = Graph.Path.Dijkstra(G)(W)

    module C = Constraint

    exception No_Path_Found (* i.e., a bug *)
      
    let rec closure_path cg lhs rhs =  
      let seen = Hashtbl.create 127 in
      let graph = G.create () in
      (*let () = Log.fixme ~ctx:(Kind.ctx lhs) "start" in
        Printable.start := Some (Kind.id lhs);
        Printable.fin := Some (Kind.id rhs);
        Printf.eprintf "start: %d, end: %d\n" (Kind.id lhs) (Kind.id rhs);*)
      let rec work l r = 
        let rid = Kind.id r in
        let lid = Kind.id l in
          (*Printf.eprintf "work: %d %d\n" lid rid;*)
          if lid == rid 
          then (*Printf.eprintf "found path\n"*) ()
          else if Hashtbl.mem seen lid
          then (*Printf.eprintf "seen %d\n" lid*) ()
          else
            let () = Hashtbl.add seen lid () in
            let succs = CG.succs cg lid in
              List.iter (handle_edge r) succs
      and handle_edge r e = match e.C.ts_origin with
        | C.Source ctx ->
            let edge = G.E.create
              (Kind.id e.C.ts_lhs) (Some(Kind.id e.C.ts_lhs, ctx)) (Kind.id e.C.ts_rhs)
            in
              (*Printf.eprintf "source %d %d\n%!"
                (Kind.id e.C.ts_lhs) (Kind.id e.C.ts_rhs);*)
              G.add_edge_e graph edge;
              work e.C.ts_rhs r
        | C.Derived e' -> 
            G.add_edge graph (Kind.id e.C.ts_lhs) (Kind.id e'.C.ts_lhs);
            (*Printf.eprintf "derived: %d -> %d (via %d -> %d) (k: %s)\n%!"
              (Kind.id e.C.ts_lhs) (Kind.id e.C.ts_rhs)
              (Kind.id e'.C.ts_lhs) (Kind.id e'.C.ts_rhs)
              (Constraint.to_string e);*)

            G.add_edge graph (Kind.id e'.C.ts_rhs) (Kind.id e.C.ts_rhs);
            work e'.C.ts_lhs e'.C.ts_rhs;
            work e.C.ts_rhs r

        | C.Closure(e1,e2) -> 
            if (Kind.id e1.C.ts_lhs) == (Kind.id e1.C.ts_rhs)
            then handle_edge r {e1 with C.ts_rhs = e.C.ts_rhs}
            else if (Kind.id e2.C.ts_lhs) == (Kind.id e2.C.ts_rhs)
            then handle_edge r {e2 with C.ts_lhs = e.C.ts_lhs}
            else ()
      in 
        work lhs rhs;
        (* DAVIDAN: MikeF, what is this for? *)
        if false then begin
          let oc2 = open_out "blah.dot" in
            Dot.output_graph oc2 graph;
            close_out oc2
        end;
        Printable.start := None;
        Printable.fin := None;
        try 
          let path,len = Dijkstra.shortest_path graph (Kind.id lhs) (Kind.id rhs) in
            List.fold_left 
              (fun ctx e -> match G.E.label e with
                 | None -> ctx
                 | Some(i, ctx') -> 
                     (*Log.err ~ctx:ctx'  "for %d" i;*)
                     Log.append ctx' ctx
              ) (Kind.ctx lhs) path
        with Not_found -> raise No_Path_Found

      
    let no_method_error cg left right methods = 
      let ctx = 
        try closure_path cg (Kind.k_closed left) (Kind.k_open right) 
        with No_Path_Found -> 
          let ctx =  Variable.vctx left in
            Log.fixme ~ctx "NO PATH FOUND?";
            ctx
      in
        match KClass.instance_class_name right with
          | None ->
              Log.err ~ctx "@[<hov>%a does not support methods %a@]"
                KClass.format_small left
                (format_comma_list Format.pp_print_string) methods
          | Some cname ->
              Log.err ~ctx "@[<hov>%a used where %s expected@, It does not support methods %a@]"
                KClass.format_small left cname
                (format_comma_list Format.pp_print_string) methods

                
end and ConTyp : Constraint_Shell
  with module ConKind = KTyp
  = 
struct
  module ConKind = KTyp
  let lhs_kind x = Kind.KTyp x
  let rhs_kind x = Kind.KTyp x
  let closable t1 t2 () = true
    
  include Build_Constraint(ConTyp)

  module C = Constraint
    
  let close cg lhs rhs ctx = match lhs.C.ts_lhs, lhs.C.ts_rhs, rhs.C.ts_rhs with
    | Kind.KTyp t1, Kind.KTyp mid, Kind.KTyp t2 -> 
	begin match Variable.deref t1, Variable.deref mid, Variable.deref t2 with
	  | Typ_Dynamic, _, _ | _, _, Typ_Dynamic -> ()

	  | Typ_Fixme, _, _ -> 
	      let ctx = Log.merge (Variable.vctx t1) ctx in
		Log.fixme ~ctx "close on !FIXME"
	  | _, Typ_Fixme, _ -> 
	      let ctx = Log.merge (Variable.vctx mid) ctx in
		Log.fixme ~ctx "close on !FIXME"
	  | _, _, Typ_Fixme -> 
	      let ctx = Log.merge (Variable.vctx t2) ctx in
		Log.fixme ~ctx "close on !FIXME"

          | _, Typ_Witness _, _ -> ()
          | _, Typ_Closed _, _ -> ()
          | _, Typ_Forall _, _ -> ()
          | _, Typ_PVar, _ -> ()
          | _, Typ_Union _, _ -> ()
          | _, Typ_Open _, _ -> ()
          | _, Typ_Tuple _, _ -> ()
          | _, Typ_Record _, _ -> ()
          | _, Typ_Inst _, _ -> ()
          | _, Typ_Param _, _
          | _, Typ_Dynamic, _
          | _, Typ_Top, _ -> ()


	  | _, Typ_Var, _ -> 
              add_constraint cg t1 t2 ~origin:(C.Closure(lhs,rhs)) ~ctx
	end

    | Kind.KTyp t1, Kind.KTyp mid, Kind.KInst t2 -> 
	ConInstance.add_constraint cg t1 t2 ~origin:(C.Closure(lhs,rhs)) ~ctx

    | Kind.KTyp t1, _, Kind.KSuper t2 -> begin match Variable.deref t1 with
	| Typ_Dynamic -> ()
	| Typ_Fixme -> Log.fixme ~ctx "close !FIXME with super constraint"
	| _ -> ConSuper.add_constraint cg t1 t2 ~origin:(C.Closure(lhs,rhs)) ~ctx
      end
    | _ -> Log.fatal Log.empty "ConTyp.close didn't get a t?"

  let union_solutions cg t lst = 
    List.fold_left
      (fun res t' ->
	 match ConTyp.valid_subtype cg t t' with
	   | `Sub_Fail -> {res with sub_fail=t'::res.sub_fail}
	   | `Sub_Partial lst -> 
               let typs =  t'::(fst res.sub_partial) in
               let deps = lst@(snd res.sub_partial) in
                 {res with sub_partial=typs,deps}
	   | `Sub_Succ -> {res with sub_succ=t'::res.sub_succ}
      ) empty_result lst

  let check_remaining_unsat cg lhs rhs e = match Variable.deref lhs, Variable.deref rhs with
    | Typ_Union _, _ -> ()
    | _, Typ_Union lst -> 
	begin match union_solutions cg lhs lst with
	  | {sub_partial=((_::_::_),_);sub_succ=[]} as res -> 
	      let ctx = List.fold_left
		(fun ctx t -> Log.in_ctx ctx "rhs: %a" KTyp.format t
		) e.C.ts_ctx (fst res.sub_partial)
	      in
	      let ctx = Log.in_ctx ctx "lhs: %a" KTyp.format lhs in
		Log.warn ~ctx "warning: union constraint not fully satisfied" 
	  | _ -> ()
	end
    | _ -> ()

  let solve_t_union self cgraph t lst union ~ctx = 
    let ctx = Log.in_ctx self.C.ts_ctx "union with lhs: %a" KTyp.format t in
      match union_solutions cgraph t lst with
        | {sub_partial=[],_;sub_succ=[]} ->
	    Constraint.set_unsat self;
	    Log.err ~ctx
	      "subtype relation failed for all members of union type"
	      
        | {sub_partial=[t'],_;sub_succ=[]} | {sub_succ=[t']} ->
	    (*Log.note "solved union with single match";*)
	    add_constraint cgraph t t' ~origin:(C.Derived self)  ~ctx
	      
        | {sub_partial=(_::_::_),deps; sub_succ=[]} ->
	    (*Log.note "no valid subtype union options yet"*)
            (* ensure the constraint solver revisits this constraint
               when one of the constituent elements of the types is
               revisited *)
            List.iter
              (fun dep ->
                 CG.fixpoint_dependency cgraph dep (Variable.vid union)
              ) deps
	    
        | {sub_succ=(t'::_::_)} ->
	    Log.fixme ~ctx "several successful candidates for union, choosing arbitrarily";
	    add_constraint cgraph t t' ~origin:(Constraint.Derived self) ~ctx
	    
  let solve cgraph t1 t2 self = 
    if conf.debug_constraints
    then Log.note ~ctx:self.C.ts_ctx "@[<v>t solving@, %a@, <=@, %a@]"
      KTyp.format t1 KTyp.format t2;
    let ctx = self.Constraint.ts_ctx in
      if (Variable.deref t1) == (Variable.deref t2) then ()
      else match Variable.deref t1, Variable.deref t2 with
	| _, Typ_Var -> ()
	| Typ_Var, _ -> ()
	| _, Typ_PVar -> ()
	| Typ_PVar, _ -> ()

        | Typ_Top, Typ_Top -> ()
        | _, Typ_Top -> ()
        | Typ_Top,_ -> Log.err ~ctx "Top is not a subtype of %a" KTyp.format t2

        | Typ_Witness(i1,_), Typ_Witness(i2,_) when i1 == i2 -> ()

	| Typ_Witness(_,b), _ -> 
            add_constraint ~origin:(Constraint.Derived self) cgraph b t2 ~ctx

        | _, Typ_Witness(_,b) -> 
            Log.err ~ctx
              "these variables are not parametric, a constraint exists between them" 

	| Typ_Dynamic, _ -> ()
	| _, Typ_Dynamic -> ()

	| Typ_Fixme, _ 
	| _, Typ_Fixme -> Log.fixme ~ctx "constraint on !FIXME"

	| Typ_Union lst,_  ->
	    List.iter 
	      (fun l -> 
		 add_constraint ~origin:(Constraint.Derived self) cgraph l t2 ~ctx
	      ) lst

	| _, Typ_Union lst -> solve_t_union self cgraph t1 lst t2 ctx
	      
	| Typ_Closed v1, Typ_Closed v2 -> 
	    ConClosed.add_constraint ~origin:(Constraint.Derived self) cgraph v1 v2 ~ctx

	| Typ_Open v1, Typ_Open v2 -> ()
	| Typ_Open v1, Typ_Closed v2 -> ()

	| Typ_Closed v1, Typ_Open v2 -> 
	    ConClosedOpen.add_constraint ~origin:(Constraint.Derived self) cgraph v1 v2 ~ctx

	| Typ_Tuple tup1, Typ_Tuple tup2 -> 
	    ConTuple.add_constraint ~origin:(Constraint.Derived self) cgraph tup1 tup2 ~ctx
	    
	| Typ_Tuple tup, (Typ_Open _ | Typ_Closed _) ->
            let arr = KTuple.promote cgraph tup ctx in
	      ConTyp.add_constraint ~origin:(Constraint.Derived self) cgraph arr t2 ~ctx

	| (Typ_Open _ | Typ_Closed _), Typ_Tuple tup ->
            let arr = KTuple.promote cgraph tup ctx in
	      ConTyp.add_constraint ~origin:(Constraint.Derived self) cgraph t1 arr ~ctx

	| Typ_Forall _ ,Typ_Forall _ ->
            Log.fixme ~ctx "forall <= forall"
	      
	| Typ_Forall(fenv,ft), Typ_Inst(ti,args,_) ->
	    Log.fixme ~ctx "forall <= inst"

	| Typ_Forall(fenv,clz), Typ_Open ou ->
	    ConClosedOpen.add_constraint ~origin:(Constraint.Derived self) cgraph clz ou ~ctx;

	| Typ_Forall(fenv,clz), Typ_Closed ecu ->
	    ConClosed.add_constraint ~origin:(Constraint.Derived self) cgraph clz ecu ~ctx;

	| Typ_Forall _, _ ->
            Log.fixme ~ctx "forall <= t"

	| _ ,Typ_Forall _ ->
            Log.fixme ~ctx "t <= forall"

	| Typ_Inst(ft,params,tr), _ -> ()
	    (*ConTyp.add_constraint ~origin:(Constraint.Derived self) cgraph tr t2 ~ctx*)

	| _, Typ_Inst(_,_,tr) -> ()
	    (*ConTyp.add_constraint ~origin:(Constraint.Derived self) cgraph t1 tr ~ctx*)

	| Typ_Record r1, Typ_Record r2 ->
            ConRecord.add_constraint ~origin:(Constraint.Derived self)
              cgraph r1 r2 ~ctx

	| (Typ_Open _ | Typ_Closed _), Typ_Record r ->
            begin match Variable.deref r with
              | Record_Access(obj,_,_) ->
                  ConTyp.add_constraint ~origin:(Constraint.Derived self) cgraph
                    t1 obj ~ctx
              | _ ->
                  let rec_hash = KRecord.promote cgraph r ctx in
                    ConTyp.add_constraint ~origin:(Constraint.Derived self) cgraph
                      t1 rec_hash ~ctx
            end
	| Typ_Record r, (Typ_Open _ | Typ_Closed _) ->
            let rec_hash = KRecord.promote cgraph r ctx in
              ConTyp.add_constraint ~origin:(Constraint.Derived self) cgraph
                rec_hash t2 ~ctx

        | Typ_Tuple tup, Typ_Record r -> 
            begin match Variable.deref r with
              | Hash _ ->
                  Log.err ~ctx "tuple used where hash expected"

              | Record_Access(array,_,map) ->
                  (*KTuple.random_access cgraph self tup map array ctx;*)
                  begin match KRecord.to_tuple cgraph ~array map ctx with
                    | Some tup' ->
                        ConTuple.add_constraint ~origin:(Constraint.Derived self)
                          cgraph tup tup' ~ctx
                    | None ->
                        let t_obj = KTuple.promote cgraph tup ctx in
                          ConTyp.add_constraint ~origin:(Constraint.Derived self)
                            cgraph t_obj array ~ctx
                  end

                      
              | Record _ ->
                  let r_obj = KRecord.promote cgraph r ctx in
                  let t_obj = KTuple.promote cgraph tup ctx in
                    ConTyp.add_constraint ~origin:(Constraint.Derived self)
                      cgraph t_obj r_obj ~ctx
                      
            end
        | Typ_Record r, Typ_Tuple tup ->
            Log.err ~ctx "trying to treat a record as a tuple?"


	| Typ_Param f1, Typ_Param f2 ->
            (* Contra *)
	    ConParam.add_constraint ~origin:(Constraint.Derived self) cgraph f2 f1 ~ctx
	| Typ_Param _, _ ->
	    Constraint.set_unsat self;
	    Log.err ~ctx "can't solve constraint with Typ_Param and %a"
	      KTyp.format t2
	| _, Typ_Param _ ->
	    Constraint.set_unsat self;
	    Log.err ~ctx "can't solve constraint with Typ_Param and %a"
	      KTyp.format t1

  let valid_subtype cg t1 t2 : valid_subtype = 
    if conf.debug_constraints
    then Log.note "valid subtype: %a <= %a" KTyp.format t1 KTyp.format t2;
    if t1 == t2 then `Sub_Succ
    else match Variable.deref t1, Variable.deref t2 with
      | Typ_Fixme, _ -> 
	  Log.fixme ~ctx:(Variable.vctx t1) "valid_subtype check on !FIXME";
	  `Sub_Fail
      | _, Typ_Fixme -> 
	  Log.fixme ~ctx:(Variable.vctx t2) "valid_subtype check on !FIXME";
	  `Sub_Fail
      | (Typ_Closed c1 | Typ_Open c1), 
          (Typ_Closed c2 | Typ_Open c2) -> 
          ConClosed.valid_subtype cg c1 c2

      | Typ_Closed c1, Typ_Var ->
          let succs = CG.succs cg (Variable.vid t2) in
          let acc = if succs = [] then (`Sub_Partial [Variable.vid t2]) else `Sub_Succ in
	    List.fold_left 
	      (fun acc e -> match e.Constraint.ts_rhs with
	         | Kind.KTyp r -> begin match Variable.deref r with
		     | Typ_Closed c2 -> valid_and acc (ConClosed.valid_subtype cg c1 c2)
		     | _ -> valid_and acc (`Sub_Partial [])
		   end
	         | _ -> valid_and (`Sub_Partial []) acc
	      ) acc succs
              
      | Typ_Var, Typ_Closed c2 ->
          (*Log.fixme "closed <= Var[%d]" (Variable.vid t2);*)
          let preds = CG.preds cg (Variable.vid t1) in
          let acc = if preds = [] then (`Sub_Partial [Variable.vid t1]) else `Sub_Succ in
	  List.fold_left 
	    (fun acc e -> match e.Constraint.ts_lhs with
	       | Kind.KTyp l -> begin match Variable.deref l with
		   | Typ_Closed c1 -> valid_and acc (ConClosed.valid_subtype cg c1 c2)
		   | _ -> valid_and acc (`Sub_Partial [])
		 end
	       | _ -> valid_and (`Sub_Partial []) acc
	    ) acc preds

      | Typ_Top, Typ_Top -> `Sub_Succ
      | _, Typ_Top -> `Sub_Succ
      | Typ_Top, _ -> `Sub_Fail

      | Typ_Var, _ -> `Sub_Partial [Variable.vid t1]
      | _, Typ_Var -> `Sub_Partial [Variable.vid t2]

      | _, Typ_Union lst ->
          List.fold_left
            (fun acc t' -> 
               valid_or acc (ConTyp.valid_subtype cg t1 t')
            ) `Sub_Fail lst

      | Typ_Union lst, _ ->
          List.fold_left
            (fun acc t' -> 
               valid_and acc (ConTyp.valid_subtype cg t' t2)
            ) `Sub_Succ lst
            
      | Typ_Inst _, _ -> `Sub_Partial [Variable.vid t1]
      | _, Typ_Inst _ -> `Sub_Partial [Variable.vid t2]

      | _ -> `Sub_Fail
          (*Log.err "valid fail: %a <= %a" KTyp.format t1 KTyp.format t2;*)
          
end and ConClosed : Constraint_Shell
  with module ConKind = KClass
  = 
struct
  module ConKind = KClass
  let lhs_kind x = Kind.KClosed x
  let rhs_kind x = Kind.KClosed x

  let closable t1 t2 () = false

  include Build_Constraint(ConClosed)
  module C = Constraint

  (* don't close constraints across a closed class *)
  (*let close cg lhs rhs ctx = ()*)
  let close cg lhs rhs ctx = match lhs.C.ts_lhs, lhs.C.ts_rhs, rhs.C.ts_rhs with
    | Kind.KClosed c, Kind.KOpen lhs_o, Kind.KOpen rhs_o ->
        assert false
	  (*ConClosed.add_constraint cg c rhs_o ~origin:(Constraint.Closure(lhs,rhs)) ~ctx*)
    | _ -> ()

  let check_remaining_unsat cg lhs rhs e = ()

  let solve cg left right self = 
    if conf.debug_constraints
    then Log.note ~ctx:self.C.ts_ctx 
      "conclosed solving %a[%d] <= %a[%d]" KClass.format left (Variable.vid left) 
      KClass.format right (Variable.vid right);
    let ctx = Log.in_ctx self.C.ts_ctx
      "@[closed solving %a <= %a@]" KClass.format_small left KClass.format_small right
    in
    let constraints = Stack.create () in
    let missing = ref [] in
    let err k = missing := k::!missing in
    let cmeths1 = KClass.all_methods_respond left in
    let cmeths2 = KClass.all_methods_respond right in
    let () = C.solve_closed_map ~err:err cmeths1 cmeths2
      ~push:(fun m1 m2 ->
               if m1.method_name <> "initialize"
	       then let ctx = Log.in_ctx ctx
		 "solving method: %s" m1.method_name
	       in
                 Stack.push 
                   (fun () ->
		      ConMethod.add_constraint ~origin:(Constraint.Derived self) ~ctx cg m1 m2
                   ) constraints
	    ) 
    in
    let () = match !missing with
      | [] -> Stack.iter (fun f -> f()) constraints
      | lst -> 
	  C.set_unsat self;
          ErrorTrace.no_method_error cg left right lst
            (*; Log.err ~ctx "full context"*)
    in

    let left_inst = KClass.get_instance_class left in 
    let left_cvars = (Variable.deref left_inst).class_vars in
    let right_inst = KClass.get_instance_class right in 
    let right_cvars = (Variable.deref right_inst).class_vars in
    let () = KClass.unify_class_vars cg left_cvars right_cvars ctx in

    let lhs_fields = KClass.all_fields left in
    let rhs_fields = KClass.all_fields right in
    let vars = 
      C.solve_open_map lhs_fields rhs_fields
	~fresh:(fun f -> 
		  Log.note "adding missing field: %s" f.field_name;
		  f)
	~push:(fun x y -> KField.unify cg x y ~ctx)
    in 
      if vars != lhs_fields
      then begin
	let f_missing = strmap_diff vars lhs_fields in
        let left_cls = Variable.deref left_inst in
	let merge f _ = Log.fatal Log.empty "solve_closed: should be disjoint %a" 
          KField.format f
        in
	  let vars = strmap_union merge f_missing left_cls.inst_vars in
            left_cls.inst_vars <- vars
      end

  let rec valid_declared_subtype lhs_cls rhs_name =     
    let subs = (Variable.deref lhs_cls).declared_subtypes in
      if List.exists
        (fun sub -> match KClass.immediate_class_name sub with
           | Some lhs_name when lhs_name=rhs_name -> true
           | _ -> valid_declared_subtype sub rhs_name
        ) subs
      then true
      else false

  let meth_set klass = 
    let meths = KClass.all_methods_respond klass in
      StrMap.fold (fun k v acc -> StrSet.add k acc) meths StrSet.empty

  let valid_subtype cg t1 t2 =
    match KClass.instance_class_name t1, KClass.instance_class_name t2 with
      | Some x, Some y -> 
	  if x=y then `Sub_Succ
          else begin match KClass.over_up t1 with
            | None -> `Sub_Fail
            | Some cls ->
                if valid_declared_subtype cls y
                then `Sub_Succ
                else `Sub_Fail
          end
      | _ -> 
          let t1_meths = meth_set t1 in
          let t2_meths = meth_set t2 in
            if StrSet.subset t2_meths t1_meths 
            then `Sub_Partial [Variable.vid t1; Variable.vid t2]
            else `Sub_Fail

end and ConClosedOpen  : Constraint_Shell
  with module ConKind = KClass
    = 
struct
  module ConKind = KClass
  let lhs_kind x = Kind.KClosed x
  let rhs_kind x = Kind.KOpen x
  let closable t1 t2 () = true
  include Build_Constraint(ConClosedOpen)

  module C = Constraint
  let close cg lhs rhs ctx = match lhs.C.ts_lhs, rhs.C.ts_rhs with
    | Kind.KClosed c, Kind.KOpen o ->
	(*ConClosedOpen.add_constraint cg c o ~origin:(Constraint.Closure(lhs,rhs)) ~ctx*)
        ()

    | _ -> ()

  let check_remaining_unsat cg lhs rhs e = ()

  let solve cgraph (left:ConKind.t) right self = 
    if conf.debug_constraints
    then Log.note "conclosedopen solving %a <= %a" KClass.format left KClass.format right;
    let ctx = self.Constraint.ts_ctx in
    let open_ = Variable.deref right in
    let pushf v v' = KTyp.unify cgraph v.field_typ v'.field_typ ~ctx in
    let missing = ref [] in
    let err k = missing := k::!missing
    in
    let constraints = Stack.create () in
    let solve = Constraint.solve_closed_map ~err:err in
    let cmeths = KClass.all_methods_respond left in
    let () =
      solve cmeths open_.class_methods
	~push:(fun m1 m2 -> 
               let ctx = Log.in_ctx ctx
		 "solving method: %s" m1.method_name
	       in
                 Stack.push
                   (fun () ->
		      ConMethod.add_constraint ~origin:(Constraint.Derived self) cgraph ~ctx m1 m2
                   ) constraints
	      )
  in
    let () = match !missing with
      | [] -> Stack.iter (fun f -> f()) constraints
      | lst ->
	  Constraint.set_unsat self;
          ErrorTrace.no_method_error cgraph left right lst
    in
    let lfields = KClass.all_fields left in
    let rfields = (*KClass.all_fields right*) open_.inst_vars in
    let vars = 
      Constraint.solve_open_map lfields rfields
	~fresh:(fun f -> f) ~push:pushf
    in 
    let left_inst = KClass.get_instance_class left in 
    let left_cvars = (Variable.deref left_inst).class_vars in
    let () = KClass.unify_class_vars cgraph left_cvars open_.class_vars ctx in
      if lfields != vars then
        let instance = Variable.deref left_inst in
	let missing = strmap_diff vars lfields in
	let merge f _ = 
	  Log.fatal Log.empty
	    "solve_closed_open: should be disjoint, but %s is in both" 
	    f.field_name 
	in
	  instance.inst_vars <- strmap_union merge missing instance.inst_vars;
	  (*CG.requeue cgraph (Variable.vid left);*)
          ()
            
  let valid_subtype cg t1 t2 =
    Log.fixme "closed/open valid_subtype";
    `Sub_Fail

end and ConTuple : Constraint_Shell with module ConKind = KTuple = 
struct
  module ConKind = KTuple
  let lhs_kind x = Kind.KTuple x
  let rhs_kind x = Kind.KTuple x
  let closable t1 t2 () = true
    
  include Build_Constraint(ConTuple)

  module C = Constraint
  let close cg lhs rhs ctx = match lhs.C.ts_lhs, rhs.C.ts_rhs with
    | Kind.KTuple t1, Kind.KTuple t2 ->
	(*ConTupleList.add_constraint cg t1 t2 ~parent:lhs ~ctx*)
	ConTuple.add_constraint cg t1 t2 ~origin:(Constraint.Closure(lhs,rhs)) ~ctx

    | _ -> 
        Log.fixme ~ctx "contuple other?"

  let check_remaining_unsat cg lhs rhs e = ()

  let solve cg left right self = 
    if conf.debug_constraints
    then Log.note "contuple solving %a <= %a" KTuple.format left KTuple.format right;
    let ctx = self.Constraint.ts_ctx in
    let add_t_con t1 t2 = 
      ConTyp.add_constraint ~origin:(Constraint.Derived self) cg t1 t2 ~ctx
    in
      match Variable.deref left, Variable.deref right with
        | Tuple_Rest, _ | _, Tuple_Rest -> ()
        | Array a1, Array a2 -> add_t_con a1 a2
        | (Tuple_Nil _|Tuple_Star _|Tuple_Cons _), Array a2 -> 
            add_t_con (KTuple.promote cg left ctx) a2
        | Array a1, (Tuple_Nil _|Tuple_Star _|Tuple_Cons _) -> 
            add_t_con a1 (KTuple.promote cg right ctx)

        | Tuple_Nil _, Tuple_Nil _ -> ()
        | Tuple_Star _, Tuple_Nil _ | Tuple_Nil _, Tuple_Star _ -> ()
            
        | Tuple_Cons _, Tuple_Nil _
        | Tuple_Nil _, Tuple_Cons _ ->
            let a1 = KTuple.promote cg left ctx in
            let a2 = KTuple.promote cg right ctx in
              add_t_con a1 a2

        | Tuple_Star(_,t1), Tuple_Star(_,t2) -> add_t_con t1 t2
            
        | Tuple_Cons(_,con_typ,rest), Tuple_Star(_, star_typ) ->
            begin match Variable.deref star_typ with
              | Typ_Tuple t -> 
                  ConTuple.add_constraint ~origin:(Constraint.Derived self) 
                    cg left t ~ctx

              | Typ_Var | Typ_PVar -> ()
              | _ -> add_t_con (KTuple.promote cg left ctx) star_typ
            end

        | Tuple_Star(_,star_typ), Tuple_Cons(_,con_typ,rest) ->
            begin match Variable.deref star_typ with
              | Typ_Tuple t -> 
                  ConTuple.add_constraint ~origin:(Constraint.Derived self) 
                    cg t right ~ctx
                    
              | Typ_Var | Typ_PVar -> ()
              | _ -> add_t_con star_typ (KTuple.promote cg right ctx) 
            end
            
        | Tuple_Cons(_,typ1,rest1), Tuple_Cons(_,typ2,rest2) -> 
            add_t_con typ1 typ2;
            ConTuple.add_constraint ~origin:(Constraint.Derived self) 
              cg rest1 rest2 ~ctx

  let valid_subtype cg t1 t2 =
    Log.fixme "tuple valid subtype";
    `Sub_Fail

end and ConMethod : Constraint_Shell
  with module ConKind = KMethod(*method_kind Variable.t*)
  = 
struct
  module ConKind = KMethod
  let lhs_kind x = Kind.KMethod x
  let rhs_kind x = Kind.KMethod x
  let closable t1 t2 () = true

  include Build_Constraint(ConMethod)

  module C = Constraint

  let close cg lhs rhs ctx = match lhs.C.ts_lhs,lhs.C.ts_rhs,rhs.C.ts_rhs with
    | Kind.KMethod m1, Kind.KMethod mid, Kind.KMethod m2 -> 
        begin match Variable.deref mid.method_kind with
          | MethodVar ->
	      ConMethod.add_constraint cg m1 m2 ~ctx ~origin:(Constraint.Closure(lhs,rhs))
          | _ -> ()
        end
    | _ -> 
	Log.fatal rhs.C.ts_ctx
	  "closing method constraint with non-method?"

  let intersection_solutions cg lst sig_r blk_r =
    List.fold_left
      (fun res (sig_l,blk_l) ->
	 let sig_sol = ConParam.valid_subtype cg sig_r.sig_args sig_l.sig_args in (* Contra*)
	 let blk_sol = ConBlock.valid_subtype cg blk_r blk_l in (* contra *)
	   match valid_and sig_sol blk_sol with
	     | `Sub_Fail -> {res with sub_fail=(sig_l,blk_l)::res.sub_fail}
	     | `Sub_Partial lst -> 
                 let typs = (sig_l,blk_l)::(fst res.sub_partial) in
                 let deps = lst@(snd res.sub_partial) in
                   {res with sub_partial=typs,deps}
	     | `Sub_Succ -> {res with sub_succ=(sig_l,blk_l)::res.sub_succ}
      ) {sub_fail=[]; sub_partial=[],[]; sub_succ=[]} lst

  let solve_inter_mono self cg left lst sig_r blk_r = 
    let ctx = 
      if conf.debug_constraints
      then Log.in_ctx self.C.ts_ctx "intersection with rhs: %a %a"
           KFunc.format sig_r KBlock.format blk_r
      else Log.in_ctx self.C.ts_ctx "intersection with rhs:"
    in
      match intersection_solutions cg lst sig_r blk_r with
      | {sub_partial=[],_;sub_succ=[]} ->
	  Constraint.set_unsat self;
	  Log.err ~ctx
	    "subtype relation failed for all members of intersection type"
      | {sub_partial=[(sg,blk)],_;sub_succ=[]} ->
          if conf.debug_constraints
	  then Log.note "single partial candidate for polymethod";
	  ConFunc.add_constraint cg sg sig_r ~origin:(Constraint.Derived self) ~ctx;
          (* flip variance for blocks *)
          ConBlock.add_constraint cg blk_r blk ~origin:(Constraint.Derived self) ~ctx

      | {sub_partial=(_::_::_),deps; sub_succ=[]} ->
          (* ensure the constraint solver revisits this constraint
             when one of the constituent elements of the types is
             revisited *)
          List.iter
            (fun dep ->
               CG.fixpoint_dependency cg dep (Variable.vid left.method_kind)
            ) deps

      | {sub_succ=x::xs} as res ->
	  if conf.debug_constraints
          then Log.note "%d successful candidates for polymethod"
	    (List.length res.sub_succ);
	  List.iter
            (fun (s,blk) -> 
               ConFunc.add_constraint cg s sig_r ~origin:(Constraint.Derived self) ~ctx;
               (* flip variance for blocks *)
               ConBlock.add_constraint cg blk_r blk ~origin:(Constraint.Derived self) ~ctx
            ) res.sub_succ

  let check_remaining_unsat cg lhs rhs e = 
    match Variable.deref lhs.method_kind, Variable.deref rhs.method_kind with
      | InterMethod(sigs), MonoMethod(sg2,blk2) ->
	  begin match intersection_solutions cg sigs sg2 blk2 with
	    | {sub_partial=(_::_::_),_; sub_succ=[]} as res -> 
	      let ctx = List.fold_left
		(fun ctx (t,blk) -> Log.in_ctx ctx "lhs: %a" KFunc.format t
		) e.C.ts_ctx (fst res.sub_partial) 
	      in
	      let ctx = Log.in_ctx ctx "rhs: %a" KMethod.format rhs in
		Log.warn ~ctx
		  "warning: intersection constraint not fully satisfied on %s" 
		  lhs.method_name
	    | _ -> ()
	  end
      | _ -> ()

  let solve cg (left:ConKind.t) (right:ConKind.t) self = 
    if conf.debug_constraints
    then Log.note "@[<v>conmethod solving (%d <= %d)@,%a@, <=@, %a@]" 
      (Variable.vid left.method_kind) (Variable.vid right.method_kind)
      KMethod.format left KMethod.format right;
    let ctx = self.C.ts_ctx in
      if not self.C.ts_already_inst 
      then match Variable.deref left.method_kind, Variable.deref right.method_kind with
	| MonoMethod(sg1,blk1),MonoMethod(sg2,blk2) ->
	    ConFunc.add_constraint cg sg1 sg2 ~origin:(Constraint.Derived self) ~ctx;
	    (* flip variance for blocks *)
	    ConBlock.add_constraint cg blk2 blk1 ~origin:(Constraint.Derived self) ~ctx
	      
	| InterMethod(sigs), MonoMethod(sg2,blk2) ->
	    solve_inter_mono self cg left sigs sg2 blk2

        | ForallMethod(env1,subm1), ForallMethod(env2,subm2) ->
            self.C.ts_already_inst <- true;
            begin match Variable.deref subm1 with
              | InterMethod _ ->
                  Log.warn "cant check intersection types yet"
              | _ -> 
                  if conf.check_annotations then
                    let mt1, mt2 = 
                      KSubstitution.instantiate_combined_witness
                        cg left.method_name 
                        (env1,subm1) (env2,subm2) ctx
                    in
                      if conf.debug_constraints
                      then Log.fixme "polymatchup: %a <= %a" KMethod.format mt1 KMethod.format mt2;
	              ConMethod.add_constraint cg mt1 mt2 ~origin:(Constraint.Derived self) ~ctx
            end

	| ForallMethod(env,subm), _ ->
	    self.C.ts_already_inst <- true;
	    let m' = 
              KSubstitution.instantiate_polymethod 
                cg env left.method_name subm ctx 
            in
	      ConMethod.add_constraint cg m' right ~origin:(Constraint.Derived self) ~ctx

	| (MonoMethod _|InterMethod _), ForallMethod(nv,subm) -> 
	    Log.err ~ctx:(Variable.vctx left.method_kind)
              ("monomorphic version of %s used as a subtype " ^^
                 "of a previously defined polymorhpic version:@,right:%a@,con:%a")
              left.method_name Log.format_ctx (Variable.vctx right.method_kind)
              Log.format_ctx ctx

	| InterMethod(sigs1), InterMethod(sigs2) ->
	    List.iter
	      (fun (sg,blk) ->
		 solve_inter_mono self cg left sigs1 sg blk
	      ) sigs2

	| MonoMethod(sig1,blk1), InterMethod(sigs) -> 
	    List.iter
	      (fun (sig2,blk2) ->
		 ConFunc.add_constraint cg sig1 sig2 ~origin:(Constraint.Derived self) ~ctx;
		 (* flip variance for blocks *)
		 ConBlock.add_constraint cg blk2 blk1 ~origin:(Constraint.Derived self) ~ctx;
	      ) sigs
	      
	| MethodVar , _
	| _, MethodVar -> ()

  let valid_subtype cg t1 t2 =
    Log.fixme "method valid subtype";
    `Sub_Fail

end and ConParam : Constraint_Shell
  with module ConKind = KParam
  = 
struct
  module ConKind = KParam
  let lhs_kind x = Kind.KParam x
  let rhs_kind x = Kind.KParam x
  let closable t1 t2 () = true
  include Build_Constraint(ConParam)

  module C = Constraint
  let close cg lhs rhs ctx = match lhs.C.ts_lhs, lhs.C.ts_rhs, rhs.C.ts_rhs with
    | Kind.KParam p1, Kind.KParam mid, Kind.KParam p2 -> 
        begin match Variable.deref mid with
          | `Param_Var ->
	      ConParam.add_constraint cg p1 p2 ~ctx ~origin:(Constraint.Closure(lhs,rhs));
          | _ -> ()
        end

      | _ -> Log.fatal Log.empty "close param with non-param?"

  let check_remaining_unsat cg lhs rhs e = ()

  let param_length (l : KParam.t) = 
    let rec work (real,def) : KParam.any_list -> string = function
      | `Param_tuple(_,_,rest)
      | `Param_t(_,rest) -> work (real+1,def) (rest :> KParam.any_list)
      | `Param_Default(_,rest) -> work (real,def+1) (rest :> KParam.any_list)
      | `Param_Star _ -> 
	  if real > 0 
	  then sprintf "at least %d arguments" real
	  else "any number of arguments"
      | `Param_Empty -> 
	  if real > 0 && def > 0
	  then sprintf "at least %d, but no more than %d arguments" real (real+def)
	  else if real > 0
	  then sprintf "exactly %d arguments" real
	  else if def > 0 
	  then sprintf "at most %d arguments" def
	  else sprintf "no arguments"
      | `Param_Var -> 
	  sprintf "unknown number of arguments"
    in work (0,0) ((Variable.deref l) : param_typ :> KParam.any_list)
	 
  let solve cgraph left right self = 
    if conf.debug_constraints
    then Log.note "@[<v>conparam solving@,%a@,<=@,%a@]" 
      KParam.format left KParam.format right;
    let ctx = self.Constraint.ts_ctx in
    (*Log.fixme "solve_params %a %a"
      (fvar format_params) f1 (fvar format_params) f2;*)
    let rec work (p1:KParam.any_list) (p2:KParam.any_list) = 
      match p1 ,p2 with
	| `Param_Var, _ -> ()
	| _, `Param_Var -> ()
	| `Param_Empty, `Param_Empty -> ()

	| `Param_Star t1, `Param_Star t2 -> 
	    ConTyp.add_constraint cgraph t1 t2 ~origin:(Constraint.Derived self) ~ctx

	| `Param_Star t, `Param_Empty
	| `Param_Empty, `Param_Star t -> ()

	| (`Param_Star t1 as rest1), 
	    (`Param_t(t2,rest2)|`Param_Default(t2,rest2)|`Param_tuple(t2,_,rest2))
	| (`Param_t(t1,rest1)|`Param_Default(t1,rest1)|`Param_tuple(t1,_,rest1)), 
	    (`Param_Star t2 as rest2) ->
	    ConTyp.add_constraint cgraph t1 t2 ~origin:(Constraint.Derived self) ~ctx;
	    work rest1 rest2

	| `Param_Empty, (`Param_t _ | `Param_tuple _)
	| (`Param_t _ | `Param_tuple _), `Param_Empty ->
	    Constraint.set_unsat self;
            if conf.debug_constraints
            then 
              Log.err ~ctx "wrong arity to function (%a) <= (%a), got %s, expected %s"
                KParam.format left
                KParam.format right
                (param_length left) 
                (param_length right)
            else
              Log.err ~ctx "wrong arity to function, got %s, expected %s"
                (param_length left) 
                (param_length right)
	        
	| `Param_Default (_, rest), `Param_Empty -> work rest p2
	    
	| `Param_Empty, `Param_Default(_,rest) -> work p1 rest

        | `Param_tuple(t1,elt1,rest1), `Param_tuple(t2,elt2,rest2) ->
	    ConTyp.add_constraint cgraph t1 t2 ~origin:(Constraint.Derived self) ~ctx;
	    ConTyp.add_constraint cgraph elt1 elt2 ~origin:(Constraint.Derived self) ~ctx;
            work rest1 rest2
            
        | `Param_tuple(t1,_,rest1), (`Param_t(t2,rest2) | `Param_Default(t2,rest2))
        | (`Param_t(t1,rest1) | `Param_Default(t1,rest1)), `Param_tuple(t2,_,rest2)  ->
	    ConTyp.add_constraint cgraph t1 t2 ~origin:(Constraint.Derived self) ~ctx;
            work rest1 rest2

	| (`Param_t(t1,rest1) | `Param_Default(t1,rest1)),
	    (`Param_t(t2,rest2) | `Param_Default(t2,rest2)) ->
	    ConTyp.add_constraint cgraph t1 t2 ~origin:(Constraint.Derived self) ~ctx;
	    work rest1 rest2
    in
      if Variable.same left right then ()
      else work 
	(Variable.deref left : param_typ :> KParam.any_list) 
	(Variable.deref right : param_typ :> KParam.any_list) 

  let valid_subtype cg (t1:KParam.t) (t2:KParam.t) =
    let rec work  (p1:KParam.any_list) (p2:KParam.any_list) : valid_subtype = match p1, p2 with
      | `Param_Var, _ -> `Sub_Partial [Variable.vid t1]
      | _, `Param_Var -> `Sub_Partial [Variable.vid t2]
	  
      | `Param_Star t1, `Param_Star t2 -> ConTyp.valid_subtype cg t1 t2

      | `Param_Star t, `Param_Empty
      | `Param_Empty, `Param_Star t -> `Sub_Succ

      | `Param_Star t1, (`Param_t(t2,rest2)|`Param_Default(t2,rest2)
        | `Param_tuple(t2,_,rest2)) ->
	  valid_and
	    (ConTyp.valid_subtype cg t1 t2)
	    (work p1 rest2)

      | (`Param_t(t1,rest1)|`Param_Default(t1,rest1) |`Param_tuple(t1,_,rest1)), 
	  `Param_Star t2 ->
	  valid_and
	    (ConTyp.valid_subtype cg t1 t2)
	    (work rest1 p2)

      | `Param_Empty, `Param_Empty -> `Sub_Succ
	  
      | `Param_Empty, (`Param_t _ | `Param_tuple _)
      | (`Param_t _ | `Param_tuple _), `Param_Empty -> `Sub_Fail
	  
      | `Param_Default (_, rest), `Param_Empty ->
	  work rest p2
	    
      | `Param_Empty, `Param_Default(_,rest) ->
	  work p1 rest

      | (`Param_t(t1,rest1)|`Param_Default(t1,rest1) | `Param_tuple(_,t1,rest1)),
	  (`Param_t(t2,rest2)|`Param_Default(t2,rest2) | `Param_tuple(_,t2,rest2)) ->
	  valid_and
	    (ConTyp.valid_subtype cg t1 t2)
	    (work rest1 rest2)
    in
      work ((Variable.deref t1) : param_typ :> KParam.any_list)
	((Variable.deref t2) : param_typ :> KParam.any_list)

end and ConBlock : Constraint_Shell
  with module ConKind = KBlock
  = 
struct
  module ConKind = KBlock
  let lhs_kind x = Kind.KBlock x
  let rhs_kind x = Kind.KBlock x
  let closable t1 t2 () = true
  include Build_Constraint(ConBlock)

  module C = Constraint

  let close cg lhs rhs ctx = match lhs.C.ts_lhs, lhs.C.ts_rhs, rhs.C.ts_rhs with
    | Kind.KBlock blk1, Kind.KBlock blk_mid, Kind.KBlock blk2 ->
        begin match Variable.deref blk_mid with
          | Block_Var ->
	      ConBlock.add_constraint cg blk1 blk2 ~origin:(Constraint.Closure(lhs,rhs)) ~ctx
          | _ -> ()
        end

    | _ -> Log.fatal Log.empty "ConBlock.close didn't get a block?"

  let check_remaining_unsat cg lhs rhs e = ()

  let solve_block cg left right origin ctx = 
    match Variable.deref left, Variable.deref right with
      | Block_None, Block_None -> ()
      | _, Block_Var -> ()
      | Block_Var, _ -> ()
          
      | Block s1, Block s2 -> 
	  ConFunc.add_constraint cg s1 s2 ~origin ~ctx
	    
      | Block_None, Block _ ->
	  (*if conf.strict_block_check 
	  then Log.err ~ctx "no block <= some block"*)
          ()

      | Block _, Block_None ->
	  (*if conf.strict_block_check 
	  then Log.err ~ctx "some block <= no block"*)
          ()

  let solve cg left right self =
    if conf.debug_constraints
    then Log.note "conblock solving %d <= %d" (Variable.vid left) (Variable.vid right);
    let ctx = self.Constraint.ts_ctx in
    let ctx = Log.in_ctx ctx "solving constraint on block" in
      solve_block cg left right (Constraint.Derived self) ctx

  let valid_subtype cg t1 t2 = match Variable.deref t1, Variable.deref t2 with
    | Block_None, Block _
    | Block _, Block_None -> `Sub_Fail
    | Block_None, Block_None -> `Sub_Succ
    | Block sg1, Block sg2 -> ConFunc.valid_subtype cg sg1 sg2
    | Block_Var, _ -> `Sub_Partial [Variable.vid t1]
    | _, Block_Var -> `Sub_Partial [Variable.vid t2]

end and ConFunc : Constraint_S with module ConKind = KFunc
=
struct
  module ConKind = KFunc

  let close cg lhs rhs ctx = ()

  let add_constraint cg s1 s2 ~origin ~ctx =
    ConTyp.add_constraint cg s1.sig_ret s2.sig_ret ~origin ~ctx;
    (* self is passed like a param, so contra variant *)
    ConTyp.add_constraint cg s2.sig_self s1.sig_self ~origin ~ctx;
    (* Contra *)
    ConParam.add_constraint cg s2.sig_args s1.sig_args ~origin ~ctx

  let sub_constraint cg t1 t2 ctx = 
    add_constraint cg t1 t2 ~origin:(Constraint.Source ctx) ~ctx
  let check_remaining_unsat cg lhs rhs e = ()

  let solve cg left right self = 
    Log.fatal Log.empty "BUG: ConFunc shouldn't ever solve!"

  let valid_subtype cg s1 s2 =
    let vret = ConTyp.valid_subtype cg s1.sig_ret s2.sig_ret in
      (* contra *)
    let vself = ConTyp.valid_subtype cg s2.sig_self s1.sig_self in
      (* Contra *)
    let vargs = ConParam.valid_subtype cg s2.sig_args s1.sig_args in
      valid_and vargs (valid_and vret vself)

end and ConInstance : Constraint_S with module ConKind = KTyp = 
struct
  module ConKind = KTyp
  let lhs_kind x = Kind.KTyp x
  let rhs_kind x = Kind.KInst x

  let valid_subtype cg t1 t2 = 
    Log.fixme "coninstance valid subtype";
    `Sub_Fail

  let closable t1 t2 () = false
  let close cg lhs rhs ctx = ()
          
  let check_remaining_unsat cg lhs rhs e = ()

  let instantiate_with_params cg fenv clz params ctx = 
    let fenv_ = Variable.deref fenv in
      if List.exists (fun x -> List.mem_assq x fenv_) params
      then None (* don't unfold until all of the type variables have been provided *)
      else
	let clz' = KSubstitution.instantiate_closed cg fenv clz ~params ctx in
	let inst_t = KTyp.of_closed clz' ctx in
	  Some(KTyp.new_instance cg inst_t ctx)

  module C = Constraint
  let solve cg left right self = 
    if conf.debug_constraints
    then Log.note "con instance solving";
    let ctx = self.C.ts_ctx in
    if not self.C.ts_already_inst 
    then let parent = left in
    let inst = right in
      (*Log.note ~ctx "solve_inst@ parent:%a@ inst: %a"
	KTyp.format_small parent KTyp.format_small inst;*)
      match Variable.deref parent, Variable.deref inst with
	| Typ_Forall(fenv,clz), Typ_Inst(_,None,tr) ->
	    self.C.ts_already_inst <- true;
	    let clz' = KSubstitution.instantiate_closed cg fenv clz ctx in
	    let inst_t = KTyp.of_closed clz' ctx in
	    let t' = KTyp.new_instance cg inst_t ctx in
	      KTyp.unify cg t' tr ctx;
	      CG.union_vars cg inst tr

	| Typ_Forall(fenv,clz), Typ_Inst(_,Some params, tr) ->
            begin match instantiate_with_params cg fenv clz params ctx with
              | None -> ()
              | Some t' ->
	          self.C.ts_already_inst <- true;
		  KTyp.unify cg t' tr ctx;
		  CG.union_vars cg inst tr
            end

	| Typ_Var, Typ_Inst _ -> ()

	| Typ_Closed ctu, Typ_Inst(_,None,tr) ->
	    self.C.ts_already_inst <- true;
	    let t' = KTyp.new_instance cg parent ctx in
	      KTyp.unify cg t' tr ctx;
	      CG.union_vars cg inst tr

	| Typ_Open cls, Typ_Inst(_,None,tr) ->
	    self.C.ts_already_inst <- true;
	    let o = KClass.create ctx ~inst:cls () in
	    let t' = KTyp.of_class o ctx in
	      (*
	      unify cg t' tr ctx;
	      CG.union_vars cg inst tr
	      *)
	      Log.fixme ~ctx "unify in ConInstance/open";
	      ignore(t')

	| Typ_Inst(_,_,tr), Typ_Inst _ ->
            ConInstance.add_constraint cg tr inst ~origin:(Constraint.Derived self) ~ctx
	| l,r -> () (* already instantiated *)

  let unify_instance cg t1 t2 ~ctx = 
    Log.fixme "unify instance"

  let format t1 t2 ppf = 
    Format.fprintf ppf "%a <= %a" KTyp.format t1 KTyp.format t2

  let add_constraint cg t1 t2 ~origin ~ctx =
    (*Log.note "con/inst %a <= %a" KTyp.format t1 KTyp.format t2;*)
    let dict = {C.solve = solve cg t1 t2;
		close = close cg;
		check_remaining_unsat = check_remaining_unsat cg t1 t2;
		unify = unify_instance cg t1 t2;
		format = format t1 t2;
                closable = closable t1 t2;
	       }
    in
    CG.add_edge cg @<
      Constraint.create (lhs_kind t1) (rhs_kind t2) dict
      (*~closable:false*) ~origin ~ctx

  let sub_constraint cg t1 t2 ctx = 
    add_constraint cg t1 t2 ~origin:(Constraint.Source ctx) ~ctx

end and ConSuper : Constraint_Shell
  with module ConKind = KTyp
  = 
struct
  module ConKind = KTyp
  let rhs_kind t = Kind.KSuper t
  let lhs_kind t = Kind.KSuper t
  let closable t1 t2 () = false

  let format t1 t2 ppf = 
    Format.fprintf ppf "%a <= %a" KTyp.format t1 KTyp.format t2
      
  type search_state = [`First | `Second]
  type result_state = [ search_state | `Found of KMethod.t]

  let rec find_super_method cls_v (state:search_state) mname meth_k : result_state = 
    let cls = Variable.deref cls_v in
    let state = 
      try 
	let meth' = StrMap.find mname cls.class_methods in
	  match state with
	    | `First -> `Second
	    | `Second -> `Found meth'
      with Not_found -> (state :> result_state)
    in
      List.fold_left
	(fun state x -> match state with
	   | `Found x -> state
	   | #search_state as s -> match KTyp.as_class x with
	       | Some c -> 
                   if (Variable.deref c).class_name = cls.class_name
                   then state
                   else find_super_method c s mname meth_k
	       | None -> state
	) state cls.class_parents
              
  let solve cg obj super_meth self = 
    let ctx = self.Constraint.ts_ctx in
    let ctx = Log.in_ctx ctx "@[solving super %a@ <=@ %a@]" 
      KTyp.format obj KTyp.format super_meth 
    in
      match Variable.deref obj, Variable.deref super_meth with
	| _, Typ_Var -> ()
	| Typ_Var, _ -> ()

	| Typ_Closed v1, (Typ_Closed v2|Typ_Open v2) -> 
            if KClass.has_instance_class v1 then
              let inst = KClass.get_instance_class v1 in
	        StrMap.iter
	          (fun mname meth -> 
		     match find_super_method inst `First mname meth with
		       | `Found meth' -> 
		           ConMethod.add_constraint cg meth' meth
			     ~origin:(Constraint.Derived self) ~ctx
		       | _ ->
		           Log.err ~ctx "didn't find super method for %s" mname
	          ) (Variable.deref v2).class_methods
            else Log.fixme "closed class with no instance pointer?"

        | Typ_Open _,_ -> () (* wait for a closure event *)

	| Typ_Inst(ft,params,tr), _ -> 
	    ConSuper.add_constraint ~origin:(Constraint.Derived self) cg tr super_meth ~ctx

	| _ ->  Log.err ~ctx "super constraint on non-typ: %a" KTyp.format obj

  let close cg left right ctx = Log.fatal ctx "consuper.close"

  let check_remaining_unsat cg t1 t2 con = ()

  let unify_super cg t1 t2 ~ctx = Log.fixme "unify super"

  let add_constraint cg t1 t2 ~origin ~ctx = 
    let dict = {Constraint.close = close cg;
		solve = solve cg t1 t2;
		check_remaining_unsat = check_remaining_unsat cg t1 t2;
		unify = unify_super cg t1 t2;
		format = format t1 t2;
                closable = closable t1 t2;
	       }
    in
    let c = Constraint.create (lhs_kind t1) (rhs_kind t2) 
      dict ~origin ~ctx
    in
      CG.add_edge cg c

  let sub_constraint cg t1 t2 ctx = 
    add_constraint cg t1 t2 ~origin:(Constraint.Source ctx) ~ctx
  let valid_subtype cg t1 t2 = 
    Log.fixme "super valid_subtype";
    `Sub_Fail

end and ConRecord : Constraint_Shell with module ConKind = KRecord = 
struct
  module ConKind = KRecord

  let lhs_kind x = Kind.KRecord x
  let rhs_kind x = Kind.KRecord x
  let closable t1 t2 () = true

  let check_remaining_unsat cg t1 t2 con = ()
    
  include Build_Constraint(ConRecord)

  module C = Constraint
  let close cg lhs rhs ctx = match lhs.C.ts_lhs, lhs.C.ts_rhs, rhs.C.ts_rhs with
    | Kind.KRecord k1, Kind.KRecord k2, Kind.KRecord k3 -> 
        begin match Variable.deref k2 with
          | Record_Access _ ->
              add_constraint cg k1 k2 ~origin:(C.Closure(lhs,rhs)) ~ctx
          | _ -> ()
        end
    | _ -> Log.fatal Log.empty "BUG: non-record in ConRecord constraint"

  let solve cg lhs rhs self = 
    let ctx = self.Constraint.ts_ctx in
      match Variable.deref lhs, Variable.deref rhs with
        | Record(_,lhs_map), Record_Access(_,_,rhs_map)
        | Record(_,lhs_map), Record(_,rhs_map) ->
            let ctx = self.Constraint.ts_ctx in
              LitMap.iter
                (fun lit rtyp ->
                   try let ltyp = LitMap.find lit lhs_map in
                     ConTyp.add_constraint cg ltyp rtyp
                       ~origin:(Constraint.Derived self) ~ctx
                   with Not_found ->
                     Log.err ~ctx "This record contains no field named %a" 
                       ErrorPrinter.format_literal lit
                ) rhs_map
        | Record _, Hash t2 ->
            let t1 = KRecord.promote cg lhs ctx in
              ConTyp.add_constraint cg ~origin:(Constraint.Derived self)
                t1 t2 ~ctx
        | Hash t1, Record _ ->
            let t2 = KRecord.promote cg rhs ctx in
              ConTyp.add_constraint cg ~origin:(Constraint.Derived self)
                t1 t2 ~ctx
        | Hash t1, Hash t2 ->
            ConTyp.add_constraint cg ~origin:(Constraint.Derived self)
              t1 t2 ~ctx

        | Record_Access _, _ -> ()
        | Hash t, Record_Access(array,hash,map) ->
            let t' = KRecord.promote cg rhs ctx in
            ConTyp.add_constraint cg ~origin:(Constraint.Derived self)
              t t' ~ctx

  let valid_subtype cg t1 t2 = 
    Log.fixme "record valid_subtype";
    `Sub_Fail

end

module Deprecated = struct
  let rec get_class cg t ctx = 
    match Variable.deref t with
      | Typ_Forall(_,cls) | Typ_Closed cls -> cls
      | Typ_Open cls -> cls
	  
      | Typ_Var -> 
	  let cls = KClass.create ctx () in
	    CG.union_vars cg t (KTyp.of_class cls ctx);
	    cls

      | Typ_Inst(_,_,tr) -> get_class cg tr ctx

      | Typ_Tuple tup ->
          let arr = KTuple.promote cg tup ctx in
            get_class cg arr ctx

      | _ -> 
	  Log.fatal ctx "tried to get class type for non-class: %a"
	    KTyp.format t

  let fresh_instance_with_params cg t tlist ctx = 
    let tr = KTyp.create ctx in
    let t' = Variable.create (Typ_Inst(t,Some tlist,tr)) ctx in
      ConInstance.add_constraint cg t t' ~ctx ~origin:(Constraint.Source ctx);
      t'
end

module Annotations = struct

  open Cfg
  open Annotation

  type annot_env = {
    gamma : (KTyp.t * KTyp.t option) StrMap.t;
    top_class : KClass.t;
    cur_class : KClass.t;
  }

  let empty_annot_env cls = 
    {gamma = StrMap.empty;
     top_class = cls;
     cur_class = cls;
    }

  let has_quant_vars env ts = 
    List.exists 
      (function 
	 | Type_Var _ -> true (*StrMap.mem s env.gamma *)
	 | _ -> false
      ) ts

  let shadow_err ctx _ _ = Log.fatal ctx "shadowed variable"

  let rec type_of_ident cg env ident ~ctx ~pos : KTyp.t = match ident with
    | TIdent_Relative str ->
	let ctx = Log.in_ctx Log.empty ~pos "annotated type %s" str in
	  KClass.lookup_constant cg str env.cur_class ctx

    | TIdent_Absolute str ->
	let ctx = Log.in_ctx Log.empty ~pos "annotated type %s" str in
	  KClass.lookup_constant cg str env.top_class ctx

    | TIdent_Scoped(ti,str) ->
	let t = Deprecated.get_class cg (type_of_ident cg env ti ~ctx ~pos) ctx in
	let ctx = Log.in_ctx Log.empty ~pos "annotated type %s" str in
	  KClass.lookup_constant cg str t ctx
            
  let rec type_of_annot_expr cg env annot ~ctx ~pos = match annot with
    | Type_Var v  ->
        let name = string_of_quant_var v in
	begin try fst (StrMap.find name env.gamma)
	with Not_found -> Log.fatal ctx "ident %s not found in gamma?" name
	end

    | Type_Ident (TIdent_Relative "NilClass") -> KTyp.create ctx

    | Type_Ident ti -> 
        KTyp.new_instance cg (type_of_ident cg env ti ~ctx ~pos) ctx

    | Type_Union ts ->
	let ts' = List.map (type_of_annot_expr cg env ~ctx ~pos) ts in
	  KTyp.of_union ts' ctx

    | Type_Object ([],[]) -> Variable.create Typ_Top ctx

    | Type_Object (fields,methods) ->
	let cls = KClass.create ctx () in
	let cls_t = KTyp.of_closed cls ctx in
        let inst = KTyp.new_instance cg cls_t ctx in
        let inst_cls = match KTyp.as_class inst with
            Some x -> x | None -> assert false
        in
	  List.iter
	    (fun m ->
	       let env, mt = refactor_method_list cg env [m] ~ctx ~pos in
		 KClass.add_method cg mt cls ctx;
	    ) methods;

          List.iter
            (fun (s,te) ->
               let typ = type_of_annot_expr cg env te ~ctx ~pos in
               let typ' = KClass.lookup_inst_var cg s inst_cls ctx in
                 KTyp.unify cg typ typ' ctx
            ) fields;
          inst

    | Type_Tuple elts ->
        let typs = List.map (type_of_annot_expr cg env ~ctx ~pos) elts in
        let array_kind = KTyp.create ctx in
        let array_t = KClass.lookup_constant cg "Array" env.top_class ctx in
        let array_inst = Deprecated.fresh_instance_with_params cg array_t [array_kind] ctx in
        let tup_t = KTuple.create ctx ~array:array_inst typs in
          List.iter (fun t -> ConTyp.sub_constraint cg t array_kind ctx) typs;
          KTyp.of_tuple tup_t ctx

    | Type_Dynamic -> Variable.create Typ_Dynamic ctx
    | Type_Fixme -> Variable.create Typ_Fixme ctx

    | Type_App(tid,ts) ->
	let t = type_of_ident cg env tid ~ctx ~pos in
	  begin match Variable.deref t with
	    | Typ_Forall _
	    | Typ_Var -> 
		let tr = KTyp.create ctx in
		let params = List.map (type_of_annot_expr cg env ~ctx ~pos) ts in
		let t' = Variable.create (Typ_Inst(t,Some params,tr)) ctx in
                  (*Log.fixme "app is %a" KTyp.format t';*)
		  if not (has_quant_vars env ts)
		  then ConInstance.add_constraint cg t t' ~ctx
                    ~origin:(Constraint.Source ctx);
		  t'
	    | _ -> 
		Log.fatal ctx
		  "trying to instantiate non-forall type: %a (%a)"
		  format_type_ident tid KTyp.format t;
	  end

    | Type_Optional _ -> Log.fatal ctx "misplaced optional argument"
    | Type_Varargs _ -> 
        Log.fatal ctx "misplaced varargs argument: %a" 
          Annotation.format_type_expr annot

    | Type_ParamList args -> 
	let args = type_of_annot_args cg env args ctx pos in
	  KTyp.of_param args ctx

  and type_of_annot_args cg env params ctx pos : KParam.t = 
    let rec dwork (acc : KTyp.t default_list_) = function
      | [] -> acc, []
	  
      | (Type_Varargs e)::xs -> 
	  if acc != `Param_Empty
	  then Log.fatal ctx "*param is not last?";
	  let t = type_of_annot_expr cg env e ~ctx ~pos in
	    dwork (`Param_Star t) xs

      | (Type_Optional e)::xs ->
	  let t = type_of_annot_expr cg env e ~ctx ~pos in
            dwork (`Param_Default(t,acc)) xs
      | lst -> acc, lst
    in
    let rec pwork (acc : KParam.params) = function
      | [] -> acc
      | (Type_Optional _ | Type_Varargs _)::_ ->
          Log.fatal ctx "BUG: out of place optional or varargs argument"
      | te::xs ->
	  let typ = type_of_annot_expr cg env te ~ctx ~pos in
            pwork (`Param_t(typ,acc)) xs
    in
      match params with
	| [Type_Var (QParam s)] ->
	  begin try match Variable.deref (fst (StrMap.find s env.gamma)) with
	    | Typ_Param p -> p
	    | _ -> Log.fatal ctx "polyadic ident %s has non-param kind" s
	  with Not_found -> Log.fatal ctx "polyadic ident %s not found in gamma?" s
	  end
	| _ ->
	    let pl, rest = dwork `Param_Empty (List.rev params) in
	    let pl = pwork (pl :> KParam.params) rest in
	      Variable.create pl ctx

  and type_of_func cg env args ret ~ctx ~pos = 
    let args = type_of_annot_args cg env args ctx pos in
    let ret = type_of_annot_expr cg env ret ~ctx ~pos in
    let self = 
      try fst (StrMap.find "self" env.gamma)
      with Not_found -> KTyp.create ctx
    in
      KFunc.create ctx ~self ~args ~ret ()

  and type_of_annot_block cg env blk ~ctx ~pos = match blk with
    | None -> KBlock.no_block ctx
    | Some (MethodSig(args,Some _,ret)) ->
        Log.fatal ctx "illegal nested block"
    | Some (MethodSig(args,None,ret)) ->
        let sg = type_of_func cg env args ret ~ctx ~pos in
          KBlock.create ~bsig:sg ctx ()

  and type_of_annot_method cg env (MethodSig(args,blk,ret)) ~ctx ~pos = 
    let blk' = type_of_annot_block cg env blk ~ctx ~pos in
    let sig' = type_of_func cg env args ret ~ctx ~pos in 
      sig',blk'

  and build_gamma cg env lgamma args ~ctx ~pos = 
    (* type variables can be mutually recrusive, thus we first create
       all of the types representing each variable with no bound, and
       then walk the list a second time adding their upper bounds *)
    let gamma = 
      List.fold_left 
        (fun acc -> function
           | (QSelf, b) -> 
               StrMap.add "self" (Variable.create Typ_PVar ctx,None) acc
	   | (QVar s, b) ->
               let ctx = Log.in_ctx ctx "type variable %s" s in
	         if StrMap.mem s acc
	         then acc
	         else StrMap.add s (Variable.create Typ_PVar ctx, None) acc
	   | (QParam s, b) ->
               let ctx = Log.in_ctx ctx "parameter variable %s" s in
	         if StrMap.mem s acc
	         then acc
	         else 
                   let t = KTyp.of_param (KParam.create ctx) ctx in
		     StrMap.add s (t,None) acc
        ) lgamma args
    in
    let env = {env with gamma=strmap_union (shadow_err ctx) env.gamma gamma} in
    let bound = function
      | None -> None
      | Some x -> Some (type_of_annot_expr cg env x ~ctx ~pos)
    in
      List.fold_left
        (fun acc (qvar,b) -> 
           let s = string_of_quant_var qvar in
           let typ,_ = StrMap.find s gamma in
             StrMap.add s (typ, bound b) acc
        ) gamma args

  and method_annot_name = function
    | TIdent_Relative s
    | TIdent_Absolute s
    | TIdent_Scoped(_,s) -> s
        
  and refactor_method_list cg env (lst:method_annotation list) ~ctx ~pos = 
    let name = match lst with
      | [] -> Log.fatal Log.empty "empty method list?"
      | (ident,_,_)::rest -> method_annot_name ident
    in
    let rec work lgamma meth_acc = function
      | [] -> lgamma, meth_acc
      | (mname,args,mt)::rest ->
	  let name' = method_annot_name mname in
	    let lgamma = build_gamma cg env lgamma args ~ctx ~pos
	    in
	      if name' <> name
	      then Log.fatal ctx "method list with differently named methods?";
	      let gamma' = strmap_union (shadow_err ctx) env.gamma lgamma in
	      let env = {env with gamma=gamma' (* ' *)} in
	        let meth = type_of_annot_method cg env mt ~ctx ~pos in
	          work lgamma (meth::meth_acc) rest
    in 
    let lgamma, sigs = work StrMap.empty [] lst in
    (*let gamma' = strmap_union (shadow_err ctx) env.gamma lgamma in*)
    let env = {env with gamma=lgamma} in
    let fenv = build_method_env cg lgamma lst in
    let mkind = union_methods fenv sigs ctx in
      env, {method_name=name;method_kind=Variable.create mkind ctx}

  (* convert the map environment back into a list, with the same
     ordering as the original annotation *)
  and build_poly_env cg gamma quants = 
    try
      List.fold_left
        (fun acc (v,b) -> (StrMap.find (string_of_quant_var v) gamma) :: acc)
        [] (List.rev quants)
    with Not_found -> assert false
      
  and build_method_env cg gamma method_annot_lst = 
    let quants = 
      List.fold_left
        (fun acc (_,qs,_) -> List.rev_append qs acc
        ) [] method_annot_lst 
    in
      build_poly_env cg gamma quants

  and union_methods fenv lst ctx : method_kind = 
    let mt = 
      match lst with
	| [] -> Log.fatal Log.empty "empty list in mk_inter_method_type?"
	| [sg,blk] -> MonoMethod(sg,blk)
	| lst -> InterMethod(lst)
    in
      match fenv with
        | [] -> mt
        | _::_ -> ForallMethod(Variable.create fenv ctx,Variable.create mt ctx)

  let mt_of_annotation cg env annot ctx pos = 
    match annot with
      | MethodType lst -> refactor_method_list cg env lst ctx pos
      | _ -> Log.fatal (Log.of_loc pos) "wrong kind of annotation for method"

  let class_of_annot_expr cg env te ~ctx ~pos = match te with
    | Type_Ident ti -> type_of_ident cg env ti ~ctx ~pos

    | _ -> Log.fatal (Log.of_loc pos) "unsupported declared subtype expression: %a"
        Annotation.format_type_expr te

  let decl_subtype_list cg env lst ~ctx ~pos = 
    List.map 
      (fun te ->
         let cls = class_of_annot_expr cg env te ~ctx ~pos in
           match KTyp.as_class cls with
             | None -> 
                 Log.fatal (Log.of_loc pos) "declared subtype %a is not yet defined? %a"
                   Annotation.format_type_expr te KTyp.format cls
             | Some c -> c
      ) lst

  let declare_subtypes cg env t subs ctx pos = 
    let cls_v = match KTyp.as_class t with
      | Some x -> x
      | None -> Log.fatal (Log.of_loc pos) "annotation type is not a class?"
    in
    let cls = Variable.deref cls_v in
    let example_inst = KTyp.new_instance cg t ctx in
    let declared_subs = decl_subtype_list cg env subs ctx pos in
      cls.declared_subtypes <- declared_subs;
      (* Verify the declared subtype: generate an instance of each
         subtype and ensure that an instance of [t] is a subtype of
         each of those *)
      List.iter
        (fun subcls -> 
           let ctx = Log.in_ctx ctx "verifying declared subtype: %s <= %s" 
             (default_opt "" (KClass.immediate_class_name cls_v))
             (default_opt "" (KClass.immediate_class_name subcls))
           in
           let t' = KTyp.new_instance cg (KTyp.of_closed subcls ctx) ctx in
             ConTyp.add_constraint cg example_inst t' ~ctx 
               ~origin:(Constraint.Source ctx)
        ) cls.declared_subtypes

  let class_module_annotation cg env (name,quants,subs) kind ctx pos = 
    let class_t = KClass.lookup_constant cg kind env.top_class ctx in
    let cls_t = KTyp.new_instance cg ~name class_t ctx in
    let cls = match KTyp.as_class cls_t with
      | None -> Log.fatal ctx "Class %s isn't object??" kind
      | Some x -> x
    in
    let env, t = match quants with
      | [] -> env, Variable.create (Typ_Closed cls) ctx
        | _ ->
            let lgamma = build_gamma cg env StrMap.empty quants ~ctx ~pos in
	    let gamma = strmap_union (shadow_err ctx) env.gamma lgamma in
	    let env = {env with gamma=gamma} in
            let fenv = Variable.create (build_poly_env cg gamma quants) ctx in
	      env, Variable.create (Typ_Forall(fenv,cls)) ctx
    in
      declare_subtypes cg env t subs ctx pos;
      env,t

  let class_of_annotation cg env annot ctx pos = 
    match annot with
      | ClassType t -> class_module_annotation cg env t "Class" ctx pos
      | _ -> Log.fatal (Log.of_loc pos) "wrong kind of annotation for module"

  let module_of_annotation cg env annot ctx pos = 
    match annot with
      | ClassType t -> class_module_annotation cg env t "Module" ctx pos
      | _ -> Log.fatal (Log.of_loc pos) "wrong kind of annotation for module"

  let annot_set_cur_class aenv t = {aenv with cur_class = t}

  let t_expr_of_annotation cg env annot ctx pos = 
    match annot with
      | ExprType t -> type_of_annot_expr cg env t ~ctx ~pos    
      | _ -> Log.fatal (Log.of_loc pos) "wrong kind of annotation for expr"

end
