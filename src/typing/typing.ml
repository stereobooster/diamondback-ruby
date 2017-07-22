
open Cfg
open Cfg_printer.ErrorPrinter
open Utils
open Printf
open Config
open RType

(* Given an absolute path, return the list of the relative subpaths. 
   For example, /a/b/c would return [a/b/c; b/c; c] 
*)
let all_subpaths path = 
  (*let path = File_loader.normalize_filename path in*)
  assert (path.[0] = '/');
  let rec work acc p = 
    let base = Filename.basename p in
    let dir = Filename.dirname p in
      if dir = p then acc
      else if base = "."
      then work acc dir
      else  match acc with
        | [] -> work (base::acc) dir
        | x::xs -> work ((Filename.concat base x)::acc) dir
  in work [] path

let c_stub file = 
  let file = try Filename.chop_extension file with Invalid_argument _ -> file in
  let file = file ^ ".so.rb" in
  let fname = Filename.basename file in
  let flat = Filename.concat conf.stub_dir fname in
    if Sys.file_exists flat then flat
    else 
      try try_each
        (fun p -> File_loader.find_in_libpath ~no_ext:true p [conf.stub_dir])
        (all_subpaths file)
      with Not_found -> Log.fatal Log.empty "Unable to locate stub file for %s\n" file

let c_handler = File_loader.Rename c_stub

type local_var = 
  | Local of KTyp.t
  | Captured of KTyp.t

type env = {
  self : KTyp.t;
  cur_class : KClass.t;
  cur_class_name : string;
  cur_method : KMethod.t option;
  cur_block : (KFunc.t * KTyp.t) option;
  top_typ : KTyp.t;
  in_module : bool;
  locals : local_var StrMap.t;
  globals : KTyp.t StrMap.t;
  cons : CG.t;
  loader : File_loader.t;
  scoper : Cfg_scope.t;
  already_typed : StrSet.t ref;
  ctx : Log.ctx;
  annot_env : Annotations.annot_env;
  main_test_var : StrSet.t;
  cur_filename : string;
}

let is_core env base = 
  let blength = String.length base in
  let clength = String.length env.cur_filename in
    clength >= blength &&
      String.sub env.cur_filename (clength - blength) blength = base

let is_base_types env = 
  is_core env "base_types.rb" || is_core env "nanotypes.rb"

let do_in_ctx ~func env pos fmt = 
  Log.kfsprintf
    (fun msg -> 
       let env' = func {env with ctx = Log.msg msg (Log.loc pos env.ctx)} in
	 {env' with ctx = env.ctx}
    ) fmt

let do_in_ctx_pair ~func env pos fmt = 
  Log.kfsprintf
    (fun msg -> 
       let env', r = func {env with ctx = Log.msg msg (Log.loc pos env.ctx)} in
	 {env' with ctx = env.ctx}, r
    ) fmt

let top_object = `ID_UScope("Object")
let top_module = `ID_UScope("Module")
let top_kernel = `ID_UScope("Kernel")

let in_ctx = Log.in_ctx

let class_type env cname pos = 
  let ctx = in_ctx (Log.loc pos env.ctx) "Looking up class name %s" cname in
  let cls = Deprecated.get_class env.cons env.top_typ ctx in
    KClass.lookup_constant env.cons cname cls ctx

let class_instance env cname pos = 
  let clazz = class_type env cname pos in
  let ctx = in_ctx (Log.loc pos env.ctx) "creating instance of %s" cname in
    (*fresh_instance env.cons clazz ctx*)
    KTyp.new_instance env.cons ~name:cname clazz ctx

let array_instance env kind pos = 
  let arr_t = class_type env "Array" pos in
    Deprecated.fresh_instance_with_params env.cons arr_t [kind] env.ctx

let hash_instance env key value pos = 
  let hash_t = class_type env "Hash" pos in
    Deprecated.fresh_instance_with_params env.cons hash_t [key;value] env.ctx

let tuple_of_param_list env params pos = 
  let ctx = env.ctx in 
  let elt = KTyp.create ctx in
  let ary = array_instance env elt pos in
  let rec work = function
    | `Param_Var -> Log.fatal Log.empty "of_param_list var?"
    | `Param_Empty -> Variable.create (Tuple_Nil ary) ctx
    | `Param_tuple(t,_,rest)
    | `Param_t(t,rest)
    | `Param_Default(t,rest) ->
        ConTyp.sub_constraint env.cons elt t ctx;
        Variable.create (Tuple_Cons(ary,t,work rest)) ctx
    | `Param_Star(t) -> 
        let star_ary = array_instance env t pos in
          ConTyp.sub_constraint env.cons star_ary ary ctx;
          Variable.create (Tuple_Star(ary,star_ary)) ctx
  in
  let tup_t = KTuple.of_tuple_list env.cons (work params) ctx in
  let tup = KTyp.of_tuple tup_t ctx in
    tup, elt
	
let bootstrap_object cgraph = 
  let ctx = Log.of_msg "bootstrapping class hierarchy" in
    (* construct the initial object type *)
  let obj_class = KClass.create ctx ~name:"Object" () in
  let obj_t = KTyp.of_closed obj_class ctx in
    (* construct the Class, Module and Kernel types *)
  let class_eigen = KClass.create ctx ~name:"Class" () in
  let class_closed = KClass.create ctx ~name:"Class" ~inst:class_eigen () in
  let class_t = KTyp.of_closed class_closed ctx in
  let () = KClass.add_parent cgraph ~parent:class_t class_eigen ~ctx in
  let () = KClass.add_parent cgraph ~parent:obj_t class_closed ~ctx in
  let mod_closed = KClass.create ctx ~name:"Module" () in
  let () = KClass.add_parent cgraph ~parent:obj_t mod_closed ~ctx in
  let mod_t = KTyp.of_closed mod_closed ctx in
  let kern_closed = KClass.create ctx ~name:"Kernel" () in
  let kern_t = KTyp.of_closed kern_closed ctx in
    (* bind the above constants in "Object"  *)
    KClass.add_constant cgraph "Object" obj_t obj_class ctx;
    KClass.add_constant cgraph "Class" class_t obj_class ctx;
    KClass.add_constant cgraph "Module" mod_t obj_class ctx;
    KClass.add_constant cgraph "Kernel" kern_t obj_class ctx;
    (* set Object to inherit from Kernel *)
    KClass.add_parent cgraph ~parent:kern_t obj_class ~ctx;
    obj_class, obj_t
(*
  let mod_holder = KClass.lookup_constant cgraph "Module" obj_class dp in
  let mod_class = KTyp.of_closed (fresh_closed "Module" dp) dp in
  let () = unify cgraph mod_holder mod_class dp in
  let kernel = lookup_constant cgraph "Kernel" obj_class dp in
    assert_named_class cgraph kernel "Kernel" dp;
*)

let empty_env fname loader =
  let cg = CG.create () in
  let top_class, top_t = bootstrap_object cg in
  let env = {
      self = top_t;
      cur_class = top_class;
      cur_class_name = "Object";
      top_typ = top_t;
      cur_method = None;
      cur_block = None;
      in_module = false;
      locals = StrMap.empty;
      globals = StrMap.empty;
      cons = cg;
      loader = loader;
      scoper = Cfg_scope.create loader;
      already_typed = ref StrSet.empty;
      ctx = Log.empty;
      annot_env = Annotations.empty_annot_env top_class;
      main_test_var = StrSet.empty;
      cur_filename = fname;
    }
  in
    env

let map_with_env f env list = 
  let env, res = 
    List.fold_left
      (fun (env,acc) x -> 
	 let env,x' = f env x in
	   env, x'::acc
      ) (env, []) list
  in env, (List.rev res)


let coercion env mname ret pos = 
  let ctx = Log.in_ctx env.ctx ~pos "coercion through %s" mname in
  let self = KTyp.create (in_ctx (Log.loc pos ctx) "self type") in
  let args = Variable.create `Param_Empty ctx in
  let func = KFunc.create ~self ~args ~ret ctx () in
  let mt = KMethod.create ctx mname ~func () in
    KTyp.of_method mt ctx

(* returns the tuple (obj,arr,inst)
   where obj = [to_a : () -> arr]
   and arr = Array<inst>
*)
let obj_with_to_a_method env pos = 
  let inst = KTyp.create env.ctx in
  let ret = array_instance env inst pos in
    (coercion env "to_a" ret pos), ret, inst

let rec n_fresh n ctx = 
  if n <= 0 then []
  else (KTyp.create ctx)::(n_fresh (n-1) ctx)

class captured_local_visitor = 
object(self)
  inherit default_visitor as super

  val mutable seen_vars = StrSet.empty
  val mutable captured_vars = StrMap.empty

  method captured_vars = captured_vars

  method visit_id = function
    | `ID_Var(`Var_Local,s) -> 
	seen_vars <- StrSet.add s seen_vars;
	Visitor.DoChildren
    | _ -> Visitor.DoChildren
      
  method private block_param_names params =
    let rec add acc = function
      | `Formal_block_id(`Var_Local,s) -> StrSet.add s acc
      | `Formal_block_id _ -> acc
      | `Formal_star s -> StrSet.add s acc
      | `Formal_rest -> acc
      | `Formal_tuple lst -> StrSet.union (self#block_param_names lst) acc
    in
      List.fold_left add StrSet.empty params

  method visit_stmt stmt = match stmt.snode with
      (* these start a new scope *)
    | Begin _ | End _ | Class _ | Module _ | Method _ -> Visitor.SkipChildren

    | MethodCall(_,{mc_cb=Some (CB_Block(formals,body))}) ->
	let param_names = self#block_param_names formals in
	let old_set = seen_vars in
	let () = seen_vars <- StrSet.empty in
	let (_:stmt) = visit_stmt (self :> cfg_visitor) body in
	let in_block = StrSet.diff seen_vars param_names in
	let captured = StrSet.inter old_set in_block in
	let map = 
	  StrSet.fold 
	    (fun s acc -> 
	       StrMap.add s (Captured (KTyp.create (Log.of_loc stmt.pos))) acc
	    ) captured captured_vars;
	in
	  captured_vars <- map;
	  seen_vars <- old_set;
          Visitor.SkipChildren

    | _ -> Visitor.DoChildren

end

let fresh_local_scope f env (stmt:stmt) =
  let cap_visitor = new captured_local_visitor in
  let (_:stmt) = visit_stmt (cap_visitor :> cfg_visitor) stmt in
  let env' = {env with locals = cap_visitor#captured_vars} in
  let env' = f env' stmt in
    {env' with locals=env.locals}
    
let lookup_local env name pos = 
  try match StrMap.find name env.locals with
    | Local x -> env, x
    | Captured x -> env, x
  with Not_found -> 
    (*Log.fixme ~ctx:env.ctx "BUG: didn't find local variable %s!" name;*)
    let t = KTyp.create env.ctx in
      {env with locals=StrMap.add name (Local t) env.locals}, t

let update_local env name t = 
  try match StrMap.find name env.locals with
    | Local _ -> {env with locals=StrMap.add name (Local t) env.locals}
    | Captured t' ->
	ConTyp.sub_constraint env.cons t t' env.ctx;
	env
  with Not_found ->
    (*Log.fatal (Some env.ctx) "didn't find local variable %s in update!" name*)
    {env with locals=StrMap.add name (Local t) env.locals}

let type_global env name default (pos:Log.pos) = 
  begin try env, StrMap.find name env.globals 
  with Not_found ->
    let ctx = in_ctx env.ctx ~pos "fresh type for global %s" name in
    let t = default ctx in
      {env with globals=StrMap.add name t env.globals}, t
	  end
    
let rec type_of_id env (id : identifier) (pos:Log.pos) = 
  do_in_ctx_pair env pos "typing id %a" format_identifier id ~func:begin fun env -> 
    match id with
      | `ID_Var(`Var_Local,name) -> lookup_local env name pos

      | `ID_Nil -> (*type_global env "nil" pos*)
          env, (KTyp.create env.ctx)

      | (`ID_True | `ID_False as sing) -> 
          let name = string_of_expr sing in
          let default (_:Log.ctx) = class_instance env "Boolean" pos in
            type_global env name default pos

      | `ID_Var(`Var_Builtin,name)
      | `ID_Var(`Var_Global,name) -> 
          type_global env name KTyp.create pos

      | `ID_Var(`Var_Instance, name) -> 
          let ctx = Log.in_ctx env.ctx "looking up field %s" name in
          let t = KTyp.create ctx in
          let field = KField.create ctx name ~t () in
          let typ' = KTyp.of_field field ctx in
	    ConTyp.sub_constraint env.cons env.self typ' ctx;
            env, t

      | `ID_Var(`Var_Class, name) -> 
	  let ctx = in_ctx env.ctx ~pos "looking up class var %s" name in      
	    env, KClass.lookup_class_var env.cons name env.cur_class ctx

      | `ID_Var(`Var_Constant, name) -> 
	  let ctx = in_ctx env.ctx ~pos "looking up constant %s" name in
	    (*Log.fixme ~ctx "non-scoped constant: %s" name;*)
	    env, KClass.lookup_constant env.cons name env.cur_class ctx
	      
      | `ID_Self -> env, env.self
      | `ID_Scope(l,name) -> 
	  let env, left = type_of_id env l pos in
	  let ctx = in_ctx env.ctx ~pos "looking up constant %s in %a(%a)" 
	    name format_identifier l KTyp.format left
	  in
	  let cls = Deprecated.get_class env.cons left ctx in
	    env, KClass.lookup_constant env.cons name cls ctx
	      
      | `ID_UScope(name) -> 
	  let ctx = in_ctx env.ctx ~pos "looking up constant ::(%s)" name in
	  let cls = Deprecated.get_class env.cons env.top_typ ctx in
	    env, KClass.lookup_constant env.cons name cls ctx

  end

let class_of_literal : literal -> string = function
  | `Lit_FixNum _ -> "Fixnum"
  | `Lit_BigNum _ -> "Bignum"
  | `Lit_Float _ ->  "Float"
  | `Lit_String _ -> "String"
  | `Lit_Atom _ ->   "Symbol"
  | `Lit_Regexp _ -> "Regexp"
  | `Lit_Array l ->  "Array"
  | `Lit_Hash l  ->  "Hash"
  | `Lit_Range _ ->  "Range"

let rec type_literal env (l:literal) pos : env * KTyp.t = match l with
  | `Lit_Array l -> tuple_literal env (l :> tuple_expr list) pos

  | `Lit_Hash l -> 
      let lst : (literal*expr) list option = List.fold_left 
        (fun acc (k,v) -> match acc, k with
         | Some lst, (#literal as lit) -> Some ((lit,v)::lst)
         | _ -> None
        ) (Some []) l
      in 
      begin match lst with
        | None
        | Some [] -> hash_literal env l pos
        | Some lst -> record_literal env lst pos
      end

  | #literal as l -> 
      let lit_class = class_of_literal l in
	env, class_instance env lit_class pos

and type_expr env (e:expr) pos : env * KTyp.t = 
  do_in_ctx_pair env pos "typing expression %a" format_expr e 
    ~func:(fun env -> match e with
	     | #literal as l -> type_literal env l pos 
	     | #identifier as id ->  type_of_id env id pos
	  )

and build_record_type env (typs:(literal*KTyp.t) list) pos =
  let key = KTyp.create env.ctx in
  let value = KTyp.create env.ctx in
  let hash = hash_instance env key value pos in
  let rcd = KRecord.create env.ctx typs ~hash in
    env, KTyp.of_record rcd env.ctx

and record_literal env lst pos = 
  let env,typs = List.fold_left
    (fun (env,acc) (lit,v) ->
       let env, t = type_expr env v pos in
         env, (lit,t)::acc
    ) (env, []) lst 
  in
    build_record_type env typs pos

and hash_literal env l pos = 
  let key = KTyp.create env.ctx in
  let value = KTyp.create env.ctx in
  let env = List.fold_left
    (fun env (e1,e2) ->
       let env, t1 = type_expr env e1 pos in
       let env, t2 = type_expr env e2 pos in
       let ctx1 = in_ctx env.ctx ~pos "typing hash key %a" format_expr e1 in
       let ctx2 = in_ctx env.ctx ~pos "typing hash value %a" format_expr e2 in
	 ConTyp.sub_constraint env.cons t1 key ctx1;
	 ConTyp.sub_constraint env.cons t2 value ctx2;
	 env
    ) env l
  in
    env, hash_instance env key value pos
  
and type_tuple_expr env (tup:tuple_expr) ~pos = match tup with
  | #expr as e -> type_expr env e pos
      
  | `Tuple te -> tuple_literal env te pos
      
  | `Star (#tuple_expr as e) -> 
      do_in_ctx_pair env pos "typing star expr: %a" format_tuple_expr e ~func:
	(fun env ->
	   let env, t = type_tuple_expr env e ~pos in
           let splatted = KTyp.splat env.cons t env.ctx in
	     env, splatted
	)

and tuple_literal env (list:tuple_expr list) pos = 
  let ctx = Log.in_ctx Log.empty ~pos "typing tuple literal" in
  let elt = KTyp.create ctx in
  let ary = array_instance env elt pos in
  let env, tup = List.fold_left
    (fun (env,acc) (x:tuple_expr) -> match x with
       | #expr as e ->
           let env, t = type_expr env e pos in
             ConTyp.sub_constraint env.cons t elt ctx;
             env, Variable.create (Tuple_Cons(ary,t,acc)) ctx
       | `Tuple lst -> 
           let env, t = tuple_literal env lst pos in
             ConTyp.sub_constraint env.cons t elt ctx;
             env, Variable.create (Tuple_Cons(ary,t,acc)) ctx
       | `Star e -> 
           let env, expr_t = type_tuple_expr env (e :> tuple_expr) pos in
           let splatted = KTyp.splat env.cons expr_t env.ctx in
             env, Variable.create (Tuple_Star(ary,splatted)) ctx
    ) (env,Variable.create(Tuple_Nil ary) ctx) (List.rev list)
  in
  let tup_t = KTuple.of_tuple_list env.cons tup ctx in
  let tup = KTyp.of_tuple tup_t ctx in
    env, tup

let obj_with_aref_method env lit ret pos = 
  let self = KTyp.create (in_ctx (Log.loc pos env.ctx) "self type") in
  let env, lit_t = type_literal env lit pos in
  let args = Variable.create (`Param_t(lit_t,`Param_Empty)) env.ctx in
  let func = KFunc.create ~self ~args ~ret env.ctx () in
  let mt = KMethod.create env.ctx "[]" ~func () in
  let clz = KTyp.of_method mt env.ctx in
    clz

let obj_with_aset_method env lit ret pos = 
  let self = KTyp.create (in_ctx (Log.loc pos env.ctx) "self type") in
  let env, lit_t = type_literal env lit pos in
  let args = Variable.create (`Param_t(lit_t,`Param_t(ret,`Param_Empty))) env.ctx in
  let func = KFunc.create ~self ~args ~ret env.ctx () in
  let mt = KMethod.create env.ctx "[]=" ~func () in
  let clz = KTyp.of_method mt env.ctx in
    clz

let record_access env acc lit arg_t pos =
  let obj = match acc with
    | `Get -> obj_with_aref_method env lit arg_t pos
    | `Set -> obj_with_aset_method env lit arg_t pos
  in
  let key = KTyp.create env.ctx in
  let value = KTyp.create env.ctx in
  let hash = hash_instance env key value pos in
  let rcd = KRecord.access ~getset_obj:obj ~hash [lit,arg_t] env.ctx in
    env, KTyp.of_record rcd env.ctx

let proc_from_block env blk pos = 
  let ctx = Log.in_ctx (Log.loc pos env.ctx) "converting &param to proc object" in
  let func = match KBlock.to_func env.cons blk ctx with
    | Some b -> b
    | None -> Log.fatal ctx "BUG: proc_from_block got NoBlock?"
  in
  let args = KFunc.get_params func in
  let ret = KFunc.get_ret func in
  let args_t = KTyp.of_param args ctx in
  let proc_cls = class_type env "Proc" pos in
  let proc_t = 
    Deprecated.fresh_instance_with_params env.cons proc_cls
      [args_t;ret] ctx 
  in
    (*Log.note "created proc from block: %a" KTyp.format proc_t;*)
    proc_t

let rec add_params_to_env env (params:any_formal list) ?block pos
    : env * KParam.t = 
  let rec add_varargs env lst = match lst with
    | [] -> env, `Param_Empty
    | `Formal_amp fname::ps -> 
	let blk = match block with
	  | None ->  Log.fatal env.ctx "BUG:no block passed to add_params_to_env??"
	  | Some b -> b
	in
	let t = proc_from_block env blk pos in
	let env = update_local env fname t in
	  if(ps != [])
	  then Log.fatal (Log.of_loc pos) "&param is not last?";
	  env, `Param_Empty

    | `Formal_star formal_name::ps ->
	let kind = KTyp.create env.ctx in
	let arr_inst = array_instance env kind pos in
	let env = update_local env formal_name arr_inst in
	let () = Log.debug ~pos "adding array param %s : %a" 
	  formal_name KTyp.format arr_inst in
	let env, rest = add_varargs env ps in
	  if rest != `Param_Empty
	  then Log.fatal env.ctx "*x is not last param?";
	  env, `Param_Star kind
	    
    | `Formal_default(s,`Lit_Atom a)::ps when substr "__rat_default_" a ->
	let ctx = in_ctx env.ctx ~pos "fresh type for default arg %s" s in
	let t = KTyp.create ctx in
	let env = update_local env s t in
	let env, rest = add_varargs env ps in
	  env, `Param_Default(t,rest)

    | `Formal_default(s,tuple)::ps ->
	let ctx = in_ctx env.ctx ~pos "type for default arg %s" s in
	let env, def_t = type_tuple_expr env tuple pos in
	let fresh = KTyp.create ctx in
	let () = ConTyp.sub_constraint env.cons def_t fresh ctx in
	let env = update_local env s fresh in
	let env, rest = add_varargs env ps in
	  env, `Param_Default(fresh, rest)

    | _ -> Log.fatal env.ctx "abnormal paramter in param list?"

  and add_normal env lst : env * KParam.params = match lst with
    | [] -> env, `Param_Empty
    | `Formal_block_id(`Var_Local, formal_name)::ps
    | `Formal_meth_id formal_name::ps -> 
        let ctx = Log.in_ctx Log.empty ~pos "formal param %s" formal_name in
        let old_ctx = env.ctx in
        let env = {env with ctx=ctx} in
	let t = KTyp.create ctx in
	  let env = update_local env formal_name t in
	  let () = Log.debug ~pos "adding param %s : %a" 
	    formal_name KTyp.format t in
	  let env, rest = add_normal env ps in
	    {env with ctx=old_ctx}, `Param_t(t,rest)

    | `Formal_block_id(k,s)::ps -> 
	let id = `ID_Var(k,s) in
	let env, t = type_of_id env id pos in
	let env, rest = add_normal env ps in
	  env, `Param_t(t,rest)

    | (`Formal_tuple l as p)::ps -> 
	do_in_ctx_pair env pos "typing formal tuple: %a" 
	  format_any_formal p ~func:begin fun env ->
	    let l = (l :> any_formal list) in
	    let env, typs = add_normal env l in
            let as_any = (typs : KParam.params :> KParam.any_list) in
	    let tup_t,tup_elt = tuple_of_param_list env as_any pos in
	    let env, rest = add_normal env ps in
	      env, `Param_tuple(tup_t,tup_elt,rest)
	  end

    | _ -> (add_varargs env lst :> env * KParam.params)
  in
    do_in_ctx_pair env pos "adding params to env" 
      ~func:(fun env -> 
	       let env, pl = add_normal env params in
		 env, Variable.create pl env.ctx
	    )

let type_method_targ env targ pos = 
  match targ with
    | Some e -> type_expr env e pos
    | None -> env, env.self

let rec flat_name (id:identifier) = match id with
  | `ID_Var(`Var_Constant,name) -> Some name
  | `ID_Self -> None
  | `ID_Scope(l,r) -> Some r
  | `ID_UScope i -> Some i
  | _ -> 
      None

let inherit_from env cls id pos = 
(*  match flat_name id with
    | None -> 
	Log.err "unable to determine name for class to inherit from: %a"
          format_identifier id ~ctx:(Log.of_loc pos)
    | Some name ->*)
	(*let parent = lookup_constant env.cons name env.top_typ ctx in*)
	let env, parent = type_of_id env id pos in
	let ctx = in_ctx env.ctx ~pos "typing inherit %a" format_identifier id in
	  KClass.add_parent env.cons ~parent cls ~ctx

let type_return_stmt env t pos = 
  match env.cur_method with
    | Some mt -> 
	let ret_t = KTyp.create env.ctx in
	let sg = KFunc.create ~self:env.self ~ret:ret_t env.ctx () in
	let name = KMethod.method_name mt in
	let mt' = KMethod.create env.ctx name ~func:sg () in
	  ConTyp.sub_constraint env.cons t ret_t env.ctx;
	  ConMethod.sub_constraint env.cons mt' mt env.ctx;
	  env
    | None -> Log.fatal Log.empty "type_return_stmt: when not in method"

let rec type_assignment env (lhs:lhs) rhs_t pos = 
  do_in_ctx env pos "typing assignment" ~func:begin fun env -> match lhs with
    | `Star (#identifier as id) ->
	let array_t = class_instance env "Array" pos in
	  type_assignment env id array_t pos

    | `Tuple ts -> assign_to_tuple env ts rhs_t pos

    | #identifier as id ->
	assign_to_id env id rhs_t pos
  end

and assign_to_id env (id:identifier) rhs_t pos = match id with
  | `ID_Var(`Var_Local,name) -> update_local env name rhs_t

  | `ID_Scope _ | `ID_UScope _ 
  | `ID_Var(`Var_Constant,_)  
  | `ID_Var(`Var_Builtin,_)
  | `ID_Var(`Var_Global,_)
  | `ID_Var((`Var_Class | `Var_Instance), _) -> 
      let env, t = type_of_id env id pos in
      let ctx = in_ctx (Log.of_loc pos) "assignment to %a" format_identifier id in
	(*KTyp.unify env.cons t rhs_t ~ctx;*)
	ConTyp.sub_constraint env.cons rhs_t t ctx;
	env
	  
  | `ID_Self -> Log.fatal env.ctx "assignment to self?"
  | `ID_Nil | `ID_True | `ID_False -> 
      Log.fatal (Log.of_loc pos) "can't assign to %a" format_identifier id

and assign_to_tuple env lst rhs_t pos = 
  let ctx = Log.in_ctx env.ctx "assigning to tuple @[<hov> %a@]" 
    (format_comma_list format_lhs) lst
  in
  let env, lhs_tup = tuple_of_lhs_list env lst pos in
    ConTyp.sub_constraint env.cons rhs_t lhs_tup ctx;
    env

and tuple_of_lhs_list env (lst:lhs list) pos = 
  let ctx = Log.in_ctx Log.empty ~pos "lhs tuple" in
  let elt = KTyp.create ctx in
  let ary = array_instance env elt pos in
  let rec work env : lhs list -> env * 'a = function
    | [] -> env, Variable.create (Tuple_Nil ary) ctx
    | (#identifier as i)::rest ->
        let t = KTyp.create ctx in
        let env = assign_to_id env i t pos in
        let env, tl = work env rest in
          ConTyp.sub_constraint env.cons elt t ctx;
          env, Variable.create (Tuple_Cons(ary,t,tl)) ctx

    | (`Tuple lst)::rest -> 
        let env, t = tuple_of_lhs_list env lst pos in
        let env, tl = work env rest in
          ConTyp.sub_constraint env.cons elt t ctx;
          env, Variable.create (Tuple_Cons(ary,t,tl)) ctx

    | [`Star (#identifier as i)] -> 
        let t = KTyp.create ctx in
        let env = assign_to_id env i t pos in
          env, Variable.create (Tuple_Star(ary,t)) ctx

    | (`Star _)::_::_ ->
        Log.fatal ctx "star element is not last in tuple?"
  in
  let env, tup = work env lst in
  let tup_t = KTuple.of_tuple_list env.cons tup ctx in
  let tuptyp = KTyp.of_tuple tup_t ctx in
    env, tuptyp

and multi_assign env (lhs:lhs) (rhs:tuple_expr) pos = match lhs, rhs with
  (* simple assignment *)
  | (#identifier as id), (#expr|#tuple) ->
      let env,rhs_t = type_tuple_expr env rhs pos in
	assign_to_id env id rhs_t pos
          
  (* x,y = rhs coerces rhs to an array and then parallel assigns *)
  | `Tuple lhs_tup, rhs ->
      let env, rhs_t = type_tuple_expr env rhs pos in
	assign_to_tuple env lhs_tup rhs_t pos

  (* "*x = e" packs e into an array and stores it in x *)
  | `Star (#identifier as id), (#expr as rhs_e) ->
      let env, rhs_t = type_expr env rhs_e pos in
      let arr_inst = array_instance env rhs_t pos in
	assign_to_id env id arr_inst pos

  (* With two stars, the lhs star is redundant *)
  | `Star (#identifier as e), `Star(rhs_star) ->
      multi_assign env e rhs pos

  (* "*x = e1,e2" or x=*e stores the rhs tuple/array into x *)
  | #identifier as id, (`Star _ as rhs_tup)
  | `Star (#identifier as id), (#tuple as rhs_tup) ->
      let env, rhs_t = type_tuple_expr env rhs_tup pos in
	assign_to_id env id rhs_t pos

let merge_env e1 e2 pos = 
  let same msg t1 t2 = 
    if t1 != t2
    then Log.fatal e1.ctx "different environments: %s" msg
    else t1 
  in
  let ctx = in_ctx e1.ctx ~pos "merging environments" in
  let merge t1 t2 = 
    if t1 == t2 then t1 
    else 
      let t' = KTyp.create ctx in
	ConTyp.sub_constraint e1.cons t1 t' ctx;
	ConTyp.sub_constraint e1.cons t2 t' ctx;
	t'
  in
  let merge_locals t1 t2 = match t1,t2 with
    | Local x, Local y -> Local (merge x y)
    | Captured x, Captured y -> KTyp.unify e1.cons x y e1.ctx; t1
    | _ -> Log.fatal e1.ctx "merging local and captured?"
  in
  let typed = StrSet.union !(e1.already_typed) !(e2.already_typed) in
  let () = e1.already_typed := typed; e2.already_typed := typed in
    {self      = same "self" e1.self e2.self;
     cur_class  = same "cur_class" e1.cur_class e2.cur_class;
     cur_class_name  = same "cur_class_name" e1.cur_class_name e2.cur_class_name;
     cur_method = same "cur_method" e1.cur_method e2.cur_method;
     cur_block = same "cur_block" e1.cur_block e2.cur_block;
     top_typ   = same "top_typ" e1.top_typ e2.top_typ;
     in_module = same "in_module" e1.in_module e2.in_module;
     cons      = same "cons" e1.cons e2.cons;
     loader    = same "loader" e1.loader e2.loader;
     scoper    = same "scoper" e1.scoper e2.scoper;
     already_typed = e1.already_typed;
     globals   = strmap_union merge e1.globals e2.globals;
     locals    = strmap_union merge_locals e1.locals e2.locals;
     ctx       = same "ctx" e1.ctx e2.ctx;
     annot_env = same "annot_env" e1.annot_env e2.annot_env;
     main_test_var = StrSet.union e1.main_test_var e2.main_test_var;
     cur_filename = same "cur_filename" e1.cur_filename e2.cur_filename;
    }

let new_class env name_id kind annot pos =
  let ctx = in_ctx env.ctx ~pos "creating/updating type %a" format_identifier name_id in
  let name = match flat_name name_id with
    | Some x -> x | None -> Log.fatal (Log.of_loc pos) "weird name for class?"
  in
  let env, new_class_t = match annot with
    | Some ann when conf.use_annotations ->
	let aenv = 
	  Annotations.annot_set_cur_class env.annot_env env.cur_class 
	in
	let aenv, typ = 
	  Annotations.class_of_annotation env.cons aenv ann ctx pos
	in
          (*
        let witness_typ = KTyp.instantiate_annotation env.cons typ ctx in
          if conf.check_annotations 
          then ConClosed.sub_constraint env.cons real witness_typ ctx;
          *)
	  {env with annot_env = aenv}, typ

    | _ -> 
	let class_class_t = class_type env kind pos in
	let clazz = KTyp.new_instance env.cons class_class_t ~name ctx in
	  env, clazz
  in
  (*let () = Log.note "new class has type %a" KTyp.format new_class_t in*)
  let env = do_in_ctx env pos "assigning class type to constant %a" format_identifier name_id
    ~func:(fun env -> 
             let env, t = type_of_id env name_id pos in 
	       KTyp.unify env.cons t new_class_t ctx;
	       env
          )
  in
    env, (Deprecated.get_class env.cons new_class_t ctx), new_class_t

let add_initialize_method env t pos =
  let mt = KMethod.create env.ctx "initialize" () in
  let cls = Deprecated.get_class env.cons t env.ctx in
    KClass.add_method env.cons mt cls env.ctx

let type_include env args pos = 
  List.iter
    (fun arg -> match arg with
       | #identifier as id -> inherit_from env env.cur_class id pos
       | _ -> Log.err "non-id in include: %a" format_star_expr arg
    ) args;
  env

let type_extend env targ args pos = 
  let ctx = Log.in_ctx (Log.loc pos env.ctx) "typing extend" in
  let env, targ_t = type_method_targ env targ pos in
  match KTyp.coerce_class env.cons targ_t ctx with
    | None -> 
        Log.err ~ctx "extend target is not a class: %a"
          KTyp.format targ_t;
        env
    | Some targ_cls -> 
        let inst_cls = KClass.get_instance_class targ_cls in
          List.iter
            (fun arg -> match arg with
               | #identifier as id -> inherit_from env inst_cls id pos
               | _ -> Log.err "non-id in extend: %a" format_star_expr arg
              ) args;
          env

let method_self_typ env targ_t pos = 
  let self = KTyp.create (in_ctx env.ctx ~pos "method self type") in
(*
    if (env.in_module && conf.module_subtype_self) ||
      ((not env.in_module) && conf.class_subtype_self)
    then begin
      let inst = instantiate_type env.cons targ_t env.ctx in
	ConTyp.sub_constraint env.cons inst self env.ctx
    end;
*)
    self
               
let add_field_reader env lhs_o args pos = 
  List.iter
    (fun x -> match x with
       | `Lit_String s
       | `Lit_Atom s ->
	   let ctx = in_ctx env.ctx ~pos "adding field reader for %s" s in
	   let field = `ID_Var(`Var_Instance, ("@" ^ s)) in
	   let self_t = KTyp.create ctx in
           let old_self = env.self in
             (* look up the instance variable in the scope of the method self *)
	   let env, field_t = type_of_id {env with self = self_t} field pos in
           let env = {env with self=old_self} in
	   let args = Variable.create `Param_Empty ctx in
	   let sg = KFunc.create ~self:self_t ~args ~ret:field_t env.ctx () in
	   let mt = KMethod.create env.ctx s ~func:sg () in
	     KClass.add_method env.cons mt env.cur_class ctx
       | _ -> 
	   Log.fixme ~ctx:(Log.of_loc pos) "unable to add accessors for non-constant: %a"
	     format_star_expr x
    ) args;
  let env, nil = type_of_id env `ID_Nil pos in
    do_opt lhs_o ~none:env ~some:(fun x -> type_assignment env x nil pos) 

let add_field_writer env lhs_o args pos = 
  List.iter
    (fun x -> match x with
       | `Lit_String s
       | `Lit_Atom s ->
	   let ctx = in_ctx env.ctx ~pos "adding field writer for %s" s in
	   let s' = s ^ "=" in
	   let field = `ID_Var(`Var_Instance, ("@" ^ s)) in
	   let self_t = KTyp.create ctx in
           let old_self = env.self in
             (* look up the instance variable in the scope of the method self *)
	   let env, field_t = type_of_id {env with self = self_t} field pos in
           let env = {env with self=old_self} in
	   let args = Variable.create (`Param_t(field_t,`Param_Empty)) env.ctx in
	   let sg = KFunc.create ~self:self_t ~args ~ret:field_t env.ctx () in
	   let mt = KMethod.create env.ctx s' ~func:sg () in
	     KClass.add_method env.cons mt env.cur_class ctx
       | _ -> 
	   Log.fixme ~ctx:(Log.of_loc pos) "unable to add accessors for non-constant: %a"
	     format_star_expr x
    ) args;
  let env, nil = type_of_id env `ID_Nil pos in
    do_opt lhs_o ~none:env ~some:(fun x -> type_assignment env x nil pos) 

let annotation_quantifies_self = function
  | None -> false
  | Some (Annotation.MethodType lst) ->
      List.exists 
        (fun (_,quants,_) ->
           List.exists (function Annotation.QSelf,_ -> true | _ -> false) quants
        ) lst
  | Some _ -> false

let rec type_stmt env (stmt:Cfg.stmt) = match stmt.snode with
  | ExnBlock(b) -> type_body env b stmt.pos

  | MethodCall(lhs_o,({mc_msg = `ID_MethodName ("require"|"load" as m);
                       mc_args = fname::_;
                       mc_cb = None}))
  | MethodCall(lhs_o,({mc_msg = `ID_MethodName ("safe_require"|"safe_load" as m);
                       mc_args = _::fname::_;
                       mc_cb = None})) ->
      (*Printf.eprintf "safe_require: %s\n"
        (format_to_string format_method_arg_expr orig);*)
      let rl = if m = "safe_require" || m = "require" then `Require else `Load in
      let env = load_file env fname rl stmt.pos in
      let bool_cls = class_type env "Boolean" stmt.pos in
      let bool = KTyp.new_instance env.cons bool_cls env.ctx in
 	do_opt lhs_o
	  ~none:env
	  ~some:(fun x -> type_assignment env x bool stmt.pos) 

  | MethodCall(lhs_o,
	       {mc_target = targ;
		mc_msg = `ID_MethodName "new";
		mc_args = args;
		mc_cb = cb}) -> type_new_stmt env lhs_o targ args cb stmt.pos

  | MethodCall(lhs_o,
	       {mc_target = targ;
		mc_msg = `ID_Super;
		mc_args = args;
		mc_cb = cb}) -> 
      assert(targ = None);
      do_in_ctx env stmt.pos "typing super statement" ~func:begin fun env ->
	type_super env lhs_o args cb stmt.pos
      end

  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "include";
		mc_args = args;
		mc_cb = None}) -> 
      let env = type_include env args stmt.pos in
	do_opt lhs_o ~none:env
	  ~some:(fun x -> type_assignment env x env.self stmt.pos) 

  | MethodCall(lhs_o,
	       {mc_target = targ;
		mc_msg = `ID_MethodName "extend";
		mc_args = args;
		mc_cb = None}) -> 
      let env = type_extend env targ args stmt.pos in
	do_opt lhs_o ~none:env
	  ~some:(fun x -> type_assignment env x env.self stmt.pos) 

  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "module_function";
		mc_args = args;
		mc_cb = None}) -> 
      List.iter
        (function
           | `Lit_Atom x -> KClass.module_function env.cons x env.cur_class env.ctx
           | _ -> Log.fixme ~ctx:(Log.of_loc stmt.pos) "non-atom passed to module_function"
        ) args;
      do_opt lhs_o ~none:env
	~some:(fun x -> type_assignment env x env.self stmt.pos)

  | MethodCall(lhs_o, {mc_msg = `ID_MethodName "module_function"}) -> 
      Log.fixme "missed call to module_function: %a" Cfg_printer.CodePrinter.format_stmt stmt;
      env
      
  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "attr";
		mc_args = [f]; (* default arg is false: read only *)
		mc_cb = None}) -> 
      add_field_reader env lhs_o [f] stmt.pos 

  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "attr";
		mc_args = [f;`ID_True];
		mc_cb = None}) -> 
      let env = add_field_writer env lhs_o [f] stmt.pos in
	add_field_reader env lhs_o [f] stmt.pos

  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "attr";
		mc_args = args;
		mc_cb = None}) -> 
      let ctx = in_ctx env.ctx ~pos:stmt.pos "typing attr" in
	Log.fixme ~ctx "missed attr call: %s" (string_of_cfg stmt);
	env

  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "attr_accessor";
		mc_args = args;
		mc_cb = None}) -> 
      let env = add_field_writer env lhs_o args stmt.pos in
	add_field_reader env lhs_o args stmt.pos

  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "attr_writer";
		mc_args = args;
		mc_cb = None}) -> add_field_writer env lhs_o args stmt.pos

  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "attr_reader";
		mc_args = args;
		mc_cb = None}) -> add_field_reader env lhs_o args stmt.pos

  | MethodCall(lhs_o,
	       {mc_target = targ;
		mc_msg = `ID_MethodName "instance_eval";
		mc_args = [];
		mc_cb = Some (CB_Block(_,body))}) -> 
      let env, self_t = type_method_targ env targ stmt.pos in
      let targ_cls = match KTyp.as_class self_t with
        | None -> KClass.create env.ctx ()
        | Some c -> c
      in
      let class_t = KClass.get_instance_class targ_cls in
        type_eval_block env lhs_o ~self_t ~class_t body stmt.pos

  | MethodCall(lhs_o,
	       {mc_target = targ;
		mc_msg = `ID_MethodName ("class_eval"|"module_eval");
		mc_args = [];
		mc_cb = Some (CB_Block(_,body))}) -> 
      let env, self_t = type_method_targ env targ stmt.pos in
      let class_t = Deprecated.get_class env.cons self_t env.ctx in
        type_eval_block env lhs_o ~self_t ~class_t body stmt.pos

  | MethodCall(lhs_o,
               ({mc_msg = `ID_MethodName "const_set";
		 mc_args = [(`Lit_Atom cname|`Lit_String cname);(#expr as rhs)]} as mc)) ->
      let (parent:identifier) = match mc.mc_target with
        | None -> `ID_Self
        | Some (#identifier as e) -> e
        | _ -> Log.fatal Log.empty "fix the missing const_set case"
      in
      let const = `ID_Scope(parent, cname) in
      let env, rhs_t = type_expr env rhs stmt.pos in
      let env = type_assignment env const rhs_t stmt.pos in
	do_opt lhs_o ~none:env
	  ~some:(fun x -> type_assignment env x rhs_t stmt.pos) 

  | MethodCall(lhs_o,{mc_msg = `ID_MethodName "const_set"}) ->
      Log.fixme ~ctx:(Log.of_loc stmt.pos) "missed const_set: %a" format_stmt stmt;
      env

  | MethodCall(lhs_o,({mc_msg = `ID_MethodName "const_get";
                       mc_args=[`Lit_Atom c]} as mc)) ->
      let (parent:identifier) = match mc.mc_target with
        | None -> `ID_Self
        | Some (#identifier as e) -> e
        | _ -> Log.fatal Log.empty "fix the missing const_get case"
      in
      let const = (`ID_Scope(parent,c)) in
      let env,rhs_t = type_expr env const stmt.pos in
        do_opt lhs_o ~none:env
          ~some:(fun x -> type_assignment env x rhs_t stmt.pos)

  | MethodCall(Some ((`ID_Var(`Var_Local,var)) as lhs),
	       ({mc_target = Some (`Lit_String s);
		mc_msg = `ID_Operator Op_EQ;
		mc_args = [`ID_Var(`Var_Builtin,"$0")];
		mc_cb = None} as mc)) when conf.single_entry ->
      (*Log.note "*** main test detected for %s" s;*)
      let env = {env with main_test_var = StrSet.add var env.main_test_var} in
      let env, mc_ret = type_methodcall env mc stmt.pos in
	type_assignment env lhs mc_ret stmt.pos

  | MethodCall(_,{mc_msg = `ID_MethodName "define_method"}) ->
      Log.err ~ctx:(Log.of_loc stmt.pos) "missed handling define_method";
      env

  | MethodCall(lhs, {mc_msg = `ID_Operator Op_ARef;mc_cb = None;
                     mc_args = [#literal as key];mc_target=targ}) ->
      let arg_t = KTyp.create env.ctx in
      let env, targ_t = type_method_targ env targ stmt.pos in
      let env, rcd = record_access env `Get key arg_t stmt.pos in
	ConTyp.sub_constraint env.cons targ_t rcd env.ctx;
        do_opt lhs ~none:env ~some:(fun x -> type_assignment env x arg_t stmt.pos)

  | MethodCall(lhs, {mc_msg = `ID_Operator Op_ASet;mc_cb = None;
                     mc_args = [(#literal as key);(#expr as arg)];mc_target=targ}) ->
      let env, arg_t = type_expr env arg stmt.pos in
      let env, targ_t = type_method_targ env targ stmt.pos in
      let env, rcd = record_access env `Set key arg_t stmt.pos in
	ConTyp.sub_constraint env.cons targ_t rcd env.ctx;
        do_opt lhs ~none:env ~some:(fun x -> type_assignment env x arg_t stmt.pos)

  | While(g,body) -> 
      let old_env = env in
	(* create a fake block to allow 'next' statements *)
      let ret = KTyp.create (in_ctx env.ctx ~pos:stmt.pos "fake 'while' return type") in
      let args = Variable.create `Param_Empty env.ctx in
      let ms = KFunc.create ~self:env.self ~args ~ret env.ctx () in
      let env,(_:KTyp.t) = type_expr env g stmt.pos in
      let env = {env with cur_block=Some(ms,ret)} in
      let body_env = type_stmt env body in
	{(merge_env body_env env stmt.pos) with cur_block=old_env.cur_block}

  | If(`ID_Var(`Var_Local,var),t,f) when StrSet.mem var env.main_test_var ->
      (*Log.note "*** main test condition detected";*)
      env

  | If(g,t,f) ->
      let env,_ = type_expr env g stmt.pos in
      let t_env = type_stmt env t in
      let f_env = type_stmt env f in
	merge_env t_env f_env stmt.pos

  | For(params,g,body) ->
      do_in_ctx env stmt.pos "typing (for %a in %a)" 
	(format_comma_list format_any_formal) (params :> any_formal list)
	format_expr g
	~func:begin fun env ->
	  let param_tup = match params with
	    | [] -> assert false
	    | [x] -> params
	    | lst -> [`Formal_tuple lst]
	  in
	  let mc = {
	    mc_target = Some g;
	    mc_msg = `ID_MethodName "each";
	    mc_args = [];
	    mc_cb = Some (CB_Block(param_tup, body))
	  } in
	  let env', mc_ret = type_methodcall env mc stmt.pos in
	    merge_env env env' stmt.pos
	end
	
  | Seq(el) -> List.fold_left type_stmt env el

  | Case(c) ->
      let env_list = 
	List.map
	  (fun (tup,body) ->
	     let env, ts = type_tuple_expr env tup stmt.pos in
	       type_stmt env body
	  ) c.case_whens
      in
      let else_env = do_opt ~none:env ~some:(type_stmt env) c.case_else in
      let rec collapse lst = match lst with
	| [] -> env
	| [e] -> e
	| e1::e2::tl ->
	    let env' = merge_env e1 e2 stmt.pos in
	      collapse (env'::tl)
      in collapse (else_env::env_list)

  | Assign(lhs,rhs) -> multi_assign env lhs rhs stmt.pos
      (*
	| Assign((#identifier as lhs), (#expr as rhs)) ->
	let env, rhs_t = type_expr env rhs stmt.pos in
	assign_to_id env lhs rhs_t stmt.pos

	| Assign(lhs, rhs) ->
	let env, rhs_t = type_tuple_expr env rhs stmt.pos in
	type_assignment env lhs rhs_t stmt.pos
      *)	    
  | Expression(#identifier as e) -> 
      let env = match stmt.annotation with
        | None -> env
        | Some t -> 
            let rhs_t = Annotations.t_expr_of_annotation env.cons 
              env.annot_env t env.ctx stmt.pos 
            in
              type_assignment env (e:>lhs) rhs_t stmt.pos 
      in
        env

  | Expression(#literal as e) ->
      (* just check to see if all we find the identifier *)
      let env, _ = type_expr env e stmt.pos in
        env

  | Return(None) -> 
      do_in_ctx env stmt.pos "typing return statement: %a" format_stmt stmt 
	~func:begin fun env ->
	  let env, t = type_of_id env `ID_Nil stmt.pos in
	    type_return_stmt env t stmt.pos
	end

  | Return(Some e) -> 
      do_in_ctx env stmt.pos "typing return statement: %a" format_stmt stmt 
	~func:begin fun env ->
	  let env, t = type_tuple_expr env e stmt.pos in
	    type_return_stmt env t stmt.pos
	end

  | Yield(lhs_o,args)  -> 
      do_in_ctx env stmt.pos "typing yield statement: %a" format_stmt stmt 
	~func:begin fun env ->
	  let env, ret_t = type_yield env args stmt.pos in
	    do_opt lhs_o
	      ~none:env
	      ~some:(fun x -> type_assignment env x ret_t stmt.pos) 
	end


  | MethodCall(lhs_o,
	       {mc_target = None;
		mc_msg = `ID_MethodName "alias_method";
		mc_args = [`Lit_Atom link;`Lit_Atom exists];
		mc_cb = None}) -> 
      KClass.alias_method env.cons ~link ~exists 
	env.cur_class env.self env.ctx;
      do_opt lhs_o
	~none:env
	~some:(fun x -> type_assignment env x env.self stmt.pos) 
        
  | MethodCall(_,{mc_msg = `ID_MethodName "alias_method"}) -> 
      Log.fixme ~ctx:(Log.of_loc stmt.pos) "unhandled alias_method";
      env

  | Alias(Alias_Method(link_msg, exist_msg)) -> 
      (*Log.note "Alias cur_class is %a @\n self is %a"
	KClass.format env.cur_class
	KTyp.format env.self;*)
      let link = format_to_string format_msg_id link_msg in
      let exists = format_to_string format_msg_id exist_msg in
	KClass.alias_method env.cons ~link ~exists 
	  env.cur_class env.self env.ctx;
	env

  | Alias(Alias_Global(v1,v2)) ->
      do_in_ctx env stmt.pos "unifying globals %a %a" 
        format_identifier (v1:>identifier) format_identifier (v2:>identifier)
        ~func:begin fun env ->
          let env, t1 = type_of_id env (v1:>identifier) stmt.pos in
          let env, t2 = type_of_id env (v2:>identifier) stmt.pos in
            KTyp.unify env.cons t1 t2 env.ctx;
            env
        end
	  
  (* These have their own local scope *)
  | Begin(el) | End(el) -> 
      fresh_local_scope type_stmt env el

  | Module(_,name_id,body) ->
      do_in_ctx env stmt.pos "module definition %a" format_identifier name_id
	~func:begin fun env -> 
	  let old_env = env in
	  let env, module_cls, module_t = new_class env name_id "Module" 
	    stmt.annotation stmt.pos 
	  in
	  (*let () = if name_id <> top_kernel
	  then inherit_from env module_cls top_module stmt.pos in*)
	  let env = {env with self = module_t; 
		       cur_class = module_cls; 
                       cur_class_name = format_to_string format_identifier name_id;
		       in_module=true} 
	  in
	  let env = fresh_local_scope type_stmt env body in
            if conf.debug_constraints
	    then Log.debug ~pos:stmt.pos "@[<v>module %a now has type@,%a@]" 
	      format_identifier name_id KTyp.format module_t;
	    {env with self = old_env.self;
	       cur_class = old_env.cur_class;
	       cur_class_name = old_env.cur_class_name;
	       annot_env = old_env.annot_env;
	       in_module = old_env.in_module;
	    }
        end

  | Class(_,MetaClass id, body) -> 
      do_in_ctx env stmt.pos "metaclass definition %a" format_identifier id 
	~func:begin fun env ->
	  let old_env = env in
	  let env,t = type_of_id env id stmt.pos in
	  let cls = Deprecated.get_class env.cons t env.ctx in
	  let eigen_cls = KClass.get_instance_class cls in
	  let env = {env with cur_class=eigen_cls;
		       self=KTyp.of_closed eigen_cls env.ctx} 
	  in
	  let env = fresh_local_scope type_stmt env body in
	    {env with self = old_env.self;
	       cur_class = old_env.cur_class;
	       annot_env = old_env.annot_env;
	    }
        end

  | Class(_,NominalClass(name_id,inh),body) -> 
      let old_env = env in
      let ctx = Log.in_ctx env.ctx ~pos:stmt.pos "class definition %a" format_identifier name_id in
      let env = {env with ctx=ctx} in
      let env, cls, new_class_t = new_class env name_id "Class" stmt.annotation stmt.pos in
      let () = match inh with
	| None -> if name_id <> top_object
	  then inherit_from env cls top_object stmt.pos
	| Some parent -> 
	    let env, parent_t = type_of_id env parent stmt.pos in
	      KClass.add_parent env.cons ~parent:parent_t
		cls ~ctx:env.ctx
      in
      let env = {env with self = new_class_t; cur_class = cls;
                   cur_class_name = format_to_string format_identifier name_id;}
      in
      let env = fresh_local_scope type_stmt env body in
        if conf.debug_constraints
	then Log.debug ~pos:stmt.pos "@[<v>class %a now has type@,%a@]"
          format_identifier name_id KTyp.format new_class_t;
	{env with self = old_env.self;
	   cur_class = old_env.cur_class;
           cur_class_name = old_env.cur_class_name;
	   annot_env = old_env.annot_env;
	   ctx = old_env.ctx;
	}
          
  | Method(Singleton_Method(targ, msg), params, body) ->
      let ctx = in_ctx env.ctx ~pos:stmt.pos "adding singleton method %a"
	format_msg_id msg
      in
      let env, targ_t = type_of_id env targ stmt.pos in
      (*let () = Log.note "adding %a to %a" format_msg msg
	KTyp.format targ_t
      in*)
      let t_cls = Deprecated.get_class env.cons targ_t ctx in
      let cls = KClass.get_instance_class t_cls in
      let add mt = KClass.add_method env.cons mt cls ctx in
      let self = KTyp.create ctx in
	type_method_def env msg self params body add stmt.annotation stmt.pos

  | Method(Instance_Method(msg), params, body) -> 
      Log.debug "typing method %a" format_msg_id msg;
      do_in_ctx env stmt.pos "adding instance method %a" format_msg_id msg 
        ~func:begin fun env ->
          let self = KTyp.create env.ctx in
          let () = 
            if annotation_quantifies_self stmt.annotation
            then ()
            else begin
              let clz = KTyp.of_closed env.cur_class env.ctx in
              let inst = KTyp.new_instance env.cons
                ~name:env.cur_class_name clz env.ctx
              in
	        ConTyp.sub_constraint env.cons inst self env.ctx
            end
          in
          let add mt = KClass.add_method env.cons mt env.cur_class env.ctx in
            type_method_def env msg self params body add stmt.annotation stmt.pos
        end
        
  | Defined(id,s) ->
      do_in_ctx env stmt.pos "in defined? statement"  ~func:begin fun env ->
	type_assignment env (id :> lhs) (KTyp.create env.ctx) stmt.pos
      end

  | Undef _ -> Log.err ~ctx:(Log.of_loc stmt.pos) "unhandled undef"; env

  | Next(tup_opt) ->
      let env, t = match tup_opt with
	| None -> type_of_id env `ID_Nil stmt.pos 
	| Some tup -> type_tuple_expr env tup stmt.pos
      in
      let ms = match env.cur_block with
	| None -> 
	    let ctx = in_ctx env.ctx ~pos:stmt.pos "next statement" in
	      Log.fatal ctx "next not in block?"
	| Some (ms,_) -> ms
      in
      let ctx = in_ctx env.ctx ~pos:stmt.pos "next statement" in
	ConTyp.sub_constraint env.cons t (KFunc.get_ret ms) ctx;
	env

  | Break(tup_opt) -> 
      let env, t = match tup_opt with
	| None -> type_of_id env `ID_Nil stmt.pos 
	| Some tup -> type_tuple_expr env tup stmt.pos
      in
      let ret = match env.cur_block with
	| None -> Log.fatal Log.empty "break not in block?"
	| Some (_,ret) -> ret
      in
      let ctx = in_ctx env.ctx ~pos:stmt.pos "break statement" in
	ConTyp.sub_constraint env.cons t ret ctx;
	env

  | MethodCall(lhs_opt, mc) -> 
      let env, mc_ret = type_methodcall env mc stmt.pos in
	do_opt lhs_opt
	  ~none:env
	  ~some:(fun x -> type_assignment env x mc_ret stmt.pos) 

  | Retry | Redo  -> env

and type_method_def' env msg self_t params body add_meth_f annot pos = 
  let old_env = env in
  let mname = format_to_string format_msg_id msg in
  let blk = KBlock.create env.ctx () in
  let env = {env with self=self_t} in
    (* we must type the arguments with self set to the receiver, not 
       the enclosing class *)
  let env, arg_typs = 
    add_params_to_env env (params :> any_formal list) ~block:blk pos 
  in 
    (* signature used to verify method implementation (this is
       constrained either with the actual method type (ext_meth_t
       below), or with the witness for a polymorphic type *)
  let int_meth_t =
    let ret_t = KTyp.create (in_ctx env.ctx ~pos "method ret type") in
    let sg = KFunc.create ~self:self_t ~args:arg_typs ~ret:ret_t env.ctx () in
      KMethod.create env.ctx mname ~func:sg ~block:blk ()
  in
    (* signature as seen by the rest of the program *)
  let ext_meth_t, env = match annot with
    | None -> 
	if conf.warn_empty_body && body.snode = (Return (Some `ID_Nil))
	then Log.fixme ~ctx:(Log.of_loc pos) "empty body and no annotation?";
	int_meth_t, env
    | Some ann -> 
	if conf.use_annotations then begin
          if conf.emit_runtime_casts && not (is_base_types env)
          then Contracts.emit_method_contract env.cur_class_name ann pos;
          let ctx = Log.in_ctx (Log.of_loc pos) "annotation for %s" mname in
	  let annot_env, mt = 
	    Annotations.mt_of_annotation env.cons env.annot_env ann ctx pos
	  in
            match KMethod.instantiate_annotation env.cons mt ctx with
              | None -> mt, {env with annot_env = annot_env}
              | Some mt_inst ->
                  (*Log.fixme "got method annot: %a" KMethod.format mt_inst;*)
                  if conf.check_annotations
                  then ConMethod.sub_constraint env.cons int_meth_t mt_inst ctx;
	          mt, {env with annot_env = annot_env}
        end else 
	  int_meth_t, env
  in
  let () = add_meth_f ext_meth_t in
  let env = {env with cur_method=Some int_meth_t} in
  let env = 
    if annot = None
    then type_stmt env body
    else if conf.check_annotations && body.snode <> (Return (Some `ID_Nil))
    then type_stmt env body
    else env
  in
    {env with cur_method = old_env.cur_method;
       self = old_env.self;
       annot_env = old_env.annot_env;
    }

and type_method_def env msg self params body add_meth_f annot pos = 
  let f env body = 
    type_method_def' env msg self params body add_meth_f annot pos
  in
    fresh_local_scope f env body

and type_new_stmt env lhs_o targ args cb_o pos =
  let oldenv = env in
  let ctx = in_ctx (Log.of_loc pos) "typing %a" 
    format_target_and_msg (targ,`ID_MethodName "new") 
  in
  let env = {env with ctx=ctx} in
  let env, targ_t = type_method_targ env targ pos in
  let obj = KTyp.new_instance env.cons targ_t env.ctx in
  let ret_t = KTyp.create (in_ctx env.ctx ~pos "initialize ret type") in
  let env, actuals = type_actuals env ret_t args pos in
  let env, blk_t = type_codeblock env ret_t cb_o pos in
  let sg = KFunc.create ~self:obj ~args:actuals ~ret:ret_t env.ctx () in
  let mt = KMethod.create env.ctx "initialize" ~func:sg ~block:blk_t () in
  let expected = KTyp.of_method mt env.ctx in
  let () = ConTyp.sub_constraint env.cons obj expected env.ctx in
  let env = match lhs_o with
    | None -> env
    | Some lhs -> type_assignment env lhs obj pos
  in
    {env with ctx=oldenv.ctx}

and type_eval_block env lhs_o ~self_t ~class_t body pos =
  let old_env = env in
  let ret = KTyp.create (in_ctx env.ctx ~pos "block return type") in
  let args = Variable.create `Param_Empty ~ctx:env.ctx in
  let ms = KFunc.create ~self:self_t ~args:args ~ret env.ctx () in
    (* block ret and inst_eval ret are the same *)
  let env = {env with self=self_t;cur_block=Some(ms,ret);
               cur_class = class_t; cur_class_name = "EVALED CLASS"} 
  in
  let env = fresh_local_scope type_stmt env body in
  let env = {env with self=old_env.self;cur_block=old_env.cur_block;
               cur_class = old_env.cur_class; 
               cur_class_name = old_env.cur_class_name} 
  in
    do_opt lhs_o ~none:env ~some:(fun x -> type_assignment env x ret pos)

and type_yield env (args:star_expr list) pos = 
  let mt = match env.cur_method with
    | None -> Log.fatal env.ctx "yield not in method??"
    | Some x -> x
  in
  let mname = KMethod.method_name mt in
  let ret_t = KTyp.create (in_ctx env.ctx ~pos "yield return type") in
  let env, actuals = type_actuals env ret_t args pos in
  let blk_self = KTyp.create (in_ctx env.ctx ~pos "yield block self type") in
  let sg = KFunc.create ~self:blk_self ~args:actuals ~ret:ret_t 
    (in_ctx env.ctx ~pos "yield actual arguments") ()
  in
  let blk = KBlock.create ~bsig:sg env.ctx () in
  let sg' = KFunc.create env.ctx () in
  let mt' = KMethod.create env.ctx mname ~func:sg' ~block:blk () in
    ConMethod.sub_constraint env.cons mt' mt env.ctx;
    env, ret_t

and type_super env lhs_o args cb_o pos =
  let ctx = env.ctx in
  let mname = match env.cur_method with
    | Some mtv -> KMethod.method_name mtv
    | None -> 
	Log.err ~ctx "super while not in method??";
	"__unknown_method"
  in
  let ret_t = KTyp.create ctx in
  let env, actuals = type_actuals env ret_t args pos in
  let env, blk_t = type_codeblock env ret_t cb_o pos in
  let sg = KFunc.create ~self:env.self ~args:actuals ~ret:ret_t ctx () in
  let meth = KMethod.create ctx mname ~func:sg ~block:blk_t () in
  let expected = KTyp.of_method meth ctx in
    ConSuper.sub_constraint env.cons env.self expected ctx;
    do_opt lhs_o ~none:env ~some:(fun x -> type_assignment env x ret_t pos)

and type_actuals env meth_ret args pos = 
  let rec work env acc = function
    | [] -> env, acc
    | p::ps -> do_in_ctx_pair env pos "typing actual argument %a" 
	format_star_expr p ~func:begin fun env ->
	  match p with
	    | #expr as e -> 
		let env, t = type_expr env e pos in
		  work env (`Param_t(t,acc)) ps
	    | `Star e -> 
		let env, t = type_expr env e pos in
		let to_a,ret,inst = obj_with_to_a_method env pos in
		let ctx = in_ctx env.ctx ~pos "to_a for *actual" in
		  Log.debug ~pos "to_a for *actual";
		  ConTyp.sub_constraint env.cons t to_a ctx;
		  work env (`Param_Star inst) ps
	end
  in 
  let env, pl = work env `Param_Empty (List.rev args) in
    env, Variable.create pl env.ctx

and type_codeblock env meth_ret blk pos = match blk with
  | None -> env, KBlock.no_block env.ctx

  | Some (CB_Arg e) -> 
      let ctx = Log.in_ctx (Log.loc pos env.ctx) "converting proc to &param" in
      let env, e_t = type_expr env e pos in
      let mc = {mc_target = Some e; 
                mc_msg = `ID_MethodName "to_proc";
                mc_args = []; mc_cb = None;}
      in
      let env, to_proc_ret = type_methodcall env mc pos in
      let args = KParam.create ctx in
      let args_t = KTyp.of_param args ctx in
      let ret = KTyp.create ctx in
      let bsig = KFunc.create ctx ~args:args ~ret () in
      let blk = KBlock.create ctx ~bsig () in
      let proc_cls = class_type env "Proc" pos in
      let proc_t = 
	Deprecated.fresh_instance_with_params env.cons proc_cls
	  [args_t;ret] ctx 
      in
	ConTyp.sub_constraint env.cons to_proc_ret proc_t env.ctx;
	env, blk

  | Some (CB_Block(formals,body)) -> 
      do_in_ctx_pair env pos "typing block" ~func:begin fun env ->
        let old_env = env in
        let env, formals = 
          add_params_to_env env (formals :> any_formal list) pos 
        in
        let ret = KTyp.create (in_ctx env.ctx ~pos "block return type") in
        let ms = KFunc.create ~self:env.self ~args:formals ~ret env.ctx () in
        let blk = KBlock.create ~bsig:ms env.ctx () in
        let env = {env with cur_block=Some(ms,meth_ret)} in
        let env = type_stmt env body in
        let locals = strmap_inter (fun t1 _ -> t1) env.locals old_env.locals in
          {env with cur_block=old_env.cur_block; locals = locals}, blk
      end
	
and type_methodcall env mc pos = 
  do_in_ctx_pair env pos "method call %a"
    format_target_and_msg (mc.mc_target,mc.mc_msg)
    ~func:begin fun env ->
      let ctx = env.ctx in
      let name = format_to_string format_msg_id mc.mc_msg in
      let env, targ_t = type_method_targ env mc.mc_target pos in
      let ret_t = KTyp.create (in_ctx ctx ~pos "method ret type") in
      let env, actuals = type_actuals env ret_t mc.mc_args pos in
      let env, blk_t = type_codeblock env ret_t mc.mc_cb pos in
      let self_t = KTyp.create (in_ctx ctx ~pos "method self type") in
      let sg = KFunc.create ~self:self_t ~args:actuals ~ret:ret_t ctx () in
      let mt = KMethod.create ctx name ~func:sg ~block:blk_t () in
      let mt_class = KTyp.of_method mt ctx in
      let expected = KTyp.new_instance env.cons mt_class ctx in
      (*let ctx = Log.in_ctx env.ctx "expected: %a@\n got %a"
	KTyp.format targ_t KTyp.format expected
      in*)
	ConTyp.sub_constraint env.cons targ_t expected ctx;
	ConTyp.sub_constraint env.cons targ_t self_t ctx;
	env, ret_t
    end

and type_body env body pos : env = 
  let env = type_stmt env body.exn_body in
  let env =
    List.fold_left
      (fun env resc ->
	let env = 
	  List.fold_left
	    (fun env -> function
	       | Rescue_Bind(tuple_expr,b) -> 
		   let name = format_to_string format_identifier b in
		     begin match tuple_expr with
                       | `ID_UScope "Object"
		       | `ID_Var(`Var_Constant,"Object") ->
	                   let cls = class_type env "Exception" pos in
			   let inst = KTyp.new_instance env.cons cls env.ctx in
			     update_local env name inst
		       | #expr as e ->
			   let env,t = type_expr env e pos in
			   let inst = KTyp.new_instance env.cons t env.ctx in
			   update_local env name inst
		       | `Tuple e_list ->
			   let env, typs = 
			     map_with_env
			       (fun env tup -> 
				  let env,t = type_tuple_expr env tup ~pos in
				    env, (KTyp.new_instance env.cons t env.ctx)
			       ) env e_list 
			   in
			   let t = KTyp.of_union typs env.ctx in
			     update_local env name t
			       
		       | `Star e ->
			   Log.fixme "handle star in rescue binder";
			   let t = KTyp.create env.ctx in
			     update_local env name t
		     end
	       | Rescue_Expr e ->
		   let acc,_ = type_tuple_expr env e pos in acc
	    ) env resc.rescue_guards
	in
	  type_stmt env resc.rescue_body
      ) env body.exn_rescue
  in
  let env = do_opt ~none:env ~some:(type_stmt env) body.exn_ensure in
  let env = do_opt ~none:env ~some:(type_stmt env) body.exn_else in
    env

and load_file env arg rl pos = match arg with
  | `Lit_String id -> 
      (*Log.note "saw require/load: %s" id ~ctx:(Log.of_loc pos);*)
      (*Printf.eprintf "load: %s\n" id;*)
      let id = Cfg_scope.special_case_yaml id in
      let no_ext = match rl with `Require -> false | `Load -> true in
      let full_name = 
	try File_loader.find_file ~no_ext env.loader id 
	with Not_found ->
	  Log.err ~ctx:env.ctx "unable to find path for require'd file: %s"
	    id;
	  "<not_found>"
      in
      if StrSet.mem full_name !(env.already_typed)
      then env
      else begin
	let old_env = env in
	let stmt = 
	  try File_loader.load_file ~no_ext env.loader id 
	  with Not_found ->
	    Log.fatal (Log.of_loc pos) "unable to load require'd file: %s" id
	in
	let () = env.already_typed := StrSet.add full_name !(env.already_typed) in
	let env = {env with cur_method = None;
		     cur_class = Deprecated.get_class env.cons env.top_typ env.ctx;
                     cur_class_name = "Object";
		     self = env.self;
		     cur_filename = full_name;
		     (*already_typed = StrSet.add full_name env.already_typed;*)
		  }
	in
	let env = type_stmt env stmt in
	  {env with
	     cur_method = old_env.cur_method;
	     cur_class = old_env.cur_class;
             cur_class_name = old_env.cur_class_name;
	     self = old_env.self;
	     cur_filename = old_env.cur_filename}
      end

  | e -> 
      Log.err ~ctx:(Log.of_loc pos) "require/load non-constant: %a"
	format_star_expr e;
      env

let type_in_env env name cfg = 
  let cfg = 
    if conf.resolve_scopes
    then Cfg_scope.resolve_scopes env.scoper cfg 
    else cfg 
  in
  let env = {env with cur_filename=name} in
    (*Log.fixme "typing@,%a" Cfg_printer.CodePrinter.format_stmt cfg;*)
    fresh_local_scope type_stmt env cfg 

let type_cfg loader name cfg = 
  let loader = File_loader.update_c_handler loader c_handler in
    type_in_env (empty_env name loader) name cfg

let solve_constraints env = 
  CG.solve_constraints env.cons

let print_type_of env name = Log.fatal Log.empty "Typing.print_type_of"
(*
  conf.debug_level <- 11;
  try
    let cls_v = Deprecated.get_class_typ env.cons env.top_typ env.ctx in
    let cls = Variable.deref cls_v in
    let field = StrMap.find name cls.class_constants in
      type_summary_pp env.cons name field.field_typ
  with Not_found ->
    Log.fatal Log.empty
      "unable to print type for %s (can't locate type, did you spell it right?"
      name
*)      
