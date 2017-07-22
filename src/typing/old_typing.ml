
open Printf
open Cfg
open Cfg.StmtRec
open Cfg_printer
open Utils

module Make(TS : TypeSystem.S) = struct 

  module NS = Namespace

  type access = 
    | Public
    | Private
    | Protected

  let string_of_access = function
    | Public -> "public"
    | Private  -> "private"
    | Protected -> "protected"
	
  let access_of_string = function
    | "public" -> Public
    | "private" -> Private
    | "protected" -> Protected
    | _ -> raise (Invalid_argument "access_of_string")

  type method_type = {
      access : access;
      arity : int;
      formals : TS.typ list;
      ret_type : TS.typ;
      yield_cb : cb_type Unify.t;
    }

  and cb_type = 
    | CB_Var
    | CB_None
    | CB_Some of method_type

  type env = {
      ns : TS.typ NS.cursor;
      acc : access;
      loader : File_loader.t;
      locals : TS.typ StrMap.t;
      globals : TS.typ StrMap.t;
      self : TS.typ;
      already_typed : StrSet.t;
      typ_env : TS.typ_env;
    }

  let empty_env = 
    let ns = NS.to_cursor NS.empty in
    let ns = NS.add_link (NS.Absolute_Scope ["Object"]) ns in
    let te = TS.empty_typ_env in
    let self_t = TS.class_typ te (NS.Absolute_Scope ["Object"]) in
      {
	ns = ns;
	acc = Public;
	loader = File_loader.create ();
	locals = StrMap.empty;
	globals = StrMap.empty;
	self = self_t; (* top-level is always "object" *)
	already_typed = StrSet.empty;
	typ_env = te;
      }

(*	
  let unify_typ pos typ1 typ2 = match typ1,typ2 with
    | MethodTyp m1, MethodTyp m2 -> 
	begin try MethodTyp (RType.unify_method m1 m2)
	  with RType.Unification msg -> 
	    Log.err ~pos:pos msg;
	    typ1
	end
    | _ -> 
	let msg = sprintf "unify_typ %s ? %s" (string_of_typ typ1)
	  (string_of_typ typ2) 
	in
	  Log.err msg ~pos:pos; typ1
*)

  let id_of_expr = function
    | #identifier as id -> id
    | _ -> assert false

  let string_of_id : identifier -> string = function
    | `ID_Var(_,s) -> s
    | `ID_Self
    | `ID_Scope _
    | `ID_UScope _ -> assert false

  let fresh_scope env = 
    {ns = env.ns; 
     acc = Public; 
     loader = env.loader;
     globals = env.globals;
     locals = StrMap.empty;
     self = env.self;
     already_typed = env.already_typed;
     typ_env = env.typ_env;
    }

(*
  let add_con env c = {env with constraints = c::env.constraints}
*)

  let type_in_scope fn env arg scope = 
    try 
      let old_scope = NS.current_scope env.ns in
      let ns' = NS.move_to_scope scope env.ns  in
      let env' = fn {env with ns=ns'} arg in
	{env' with ns = NS.move_to_scope old_scope env'.ns}

    with Not_found ->
      let msg = sprintf "unable to enter scope %s at %s"
	(NS.string_of_scope scope) (NS.string_of_curs env.ns)
      in
	Log.err msg;
	env

  let rec lookup_id_type env (id:Cfg.identifier) = (*match id with
    | `ID_Var(`Var_Local,s) -> begin
	  try env, StrMap.find s env.locals
	  with Not_found ->
	    let t = RType.fresh() in
	    let locals = StrMap.add s t env.locals in
	      {env with locals=locals}, t
      end
    | `ID_Var(`Var_Global,s) -> begin
	  try env, StrMap.find s env.globals
	  with Not_found ->
	    let t = RType.fresh() in
	    let globals = StrMap.add s t env.globals in
	      {env with globals=globals}, t
      end

    | `ID_Var(_,s) ->
	begin match NS.find s env.ns with
	  | None -> 
	      let t = TS.fresh_typ () in
	      let ns = NS.add s (VarTyp t) env.ns in
		{env with ns=ns}, t

	  | Some(VarTyp t) -> env, t
	  | Some(ClassTyp s) -> env, (RType.of_scope s)
	  | Some(ModuleTyp s) -> env, (RType.of_scope s)
	  | Some(MethodTyp mt) -> 
	      if NS.at_root env.ns then begin
		  Log.err ("name error: " ^ s) ~pos:pos;
		  env, (RType.fresh())
		end
	      else
		let old_scope = NS.current_scope env.ns in
		let env = {env with ns = NS.leave_scope env.ns} in
		let env,t = lookup_id_type env id pos in
		  {env with ns = NS.move_to_scope old_scope env.ns}, t
	end

    | `ID_Scope(`ID_Self,id2) -> 
	Log.err ~pos:pos "handle scoped self::";
	env, (RType.fresh())

    | `ID_Scope(id1,id2) -> 
	let s = NS.scope_of_identifier id1 in
	  begin try
	      let old_scope = NS.current_scope env.ns in
	      let env = {env with ns = NS.move_to_scope s env.ns} in
	      let env,t = lookup_id_type env id2 pos in
		{env with ns = NS.move_to_scope old_scope env.ns}, t
	    with Not_found -> 
	      Log.err ("unable to locate identifier: " ^ (NS.string_of_scope s)) ~pos:pos;
	      env, (RType.fresh())
	  end
	    
    | `ID_UScope id  ->
	let old_scope = NS.current_scope env.ns in
	let env = {env with ns = NS.move_to_scope NS.root env.ns} in
	let env,t = lookup_id_type env id pos in
	  {env with ns = NS.move_to_scope old_scope env.ns}, t

    | `ID_Self -> env, env.self
						       *)
    env, (TS.fresh_typ env.typ_env)

  let type_expr env (expr:expr) : env * TS.typ = match expr with
    | #literal as l -> env, (TS.literal_typ env.typ_env l)
    | #identifier as id -> lookup_id_type env id

  let type_expr_list env el = 
    List.fold_left
      (fun (env,acc) e ->
	 let env,t = type_expr env e  in
	   env, t::acc
      ) (env,[]) el

  let type_methodcall env mc pos = 
    let _ = match mc.mc_target with
      | `No_Targ -> env, env.self
      | #expr as e -> type_expr env e 
    in
    let env,arg_typs = type_expr_list env mc.mc_args in
      env, (TS.fresh_typ env.typ_env)
(*
    let env, targ_t = type_expr env mc.mc_target pos in
      match Unify.value targ_t with
	| RType.OpenClass _
	| RType.Var _ -> 
	    let env, args_t = 
	      List.fold_left
		(fun (env,ts) x -> 
		  let env,t = type_expr env x pos in
		    env, t::ts
		) (env,[]) mc.mc_args 
	    in
	    let _ = RType.method_typ_from_args args_t in
	      env, (RType.fresh())
		(*
		  let msg = string_of_format format_msg mc.mc_msg in
		  let arity = List.length mc.mc_args in
		  let ms = RType.fresh_method arity env.acc in
		  let ct = {
		  RType.methods = StrMap.add msg ms StrMap.empty; 
		  instance_vars = StrMap.empty;
		  class_vars = StrMap.empty; }
		  in
		  let t = Unify.create (RType.OpenClass(Unify.create ct)) in
		  add_con env (Con_Unify(t, targ_t, pos))
		*)
	| RType.NamedClass s -> 
	    let msg' = string_of_format format_msg mc.mc_msg in
	    let env = add_con env (Con_HasMethod(msg',s,pos)) in
	      env, (RType.fresh ())

	(*
	  begin
	  let msg' = string_of_format format_msg mc.mc_msg in
	  try
	  let curs' = NS.move_to_scope s env.ns in
	  let _ = NS.lookup_scope (NS.Relative_Scope [msg']) curs' in
	  env
	  with Not_found ->
	  if mc.mc_msg = `ID_MethodName "new" then
	  (Log.warn "handle 'new' method";env)
	  else
	  let emsg = sprintf "while typing %s, didn't find method %s in scope %s"
	  (string_of_format format_method_call mc) msg' (NS.string_of_scope s)
	  in
	  Log.err emsg ~pos:pos;
	  env
	  end
	*)
	| RType.Tuple _
	| RType.MetaClass _ -> *) 

  let update_access (meth_expr:expr) new_acc curs = assert false (*
    let do_update name curs = 
      match NS.find name curs with
	| Some(MethodTyp mt) ->
	    let meth_typ = MethodTyp {mt with access = new_acc} in
	      NS.replace name meth_typ curs
	| Some _ -> Log.err "access method applied to non-method?";assert false
	| None -> 
	    let msg = sprintf "unable to find %s at %s to update access"
	      name (NS.string_of_curs curs)
	    in Log.err msg; curs
    in match meth_expr with
      | #identifier as meth_id -> begin
	    let scope,name = NS.scope_of_identifier_split meth_id in
	    let old_scope = NS.current_scope curs in
	    let curs = NS.move_to_scope scope curs in
	      NS.move_to_scope old_scope (do_update name curs)
	end

      | `Lit_Atom name
      | `Lit_String name -> do_update name curs

      | #literal -> assert false
								 *)

  let rec type_tuple_expr env (texpr:tuple_expr) pos : env * TS.typ = 
    match texpr with
      | #expr as e -> type_expr env e
      | `Star _ -> assert false (*Log.err ~pos:pos "handle tuple star";*)
      | `Tuple(lst) -> 
	  let env,t_list = 
	    List.fold_left (fun (env,lst) e ->
	      let env,t = type_tuple_expr env e pos in
		env,(t::lst)
			   ) (env,[]) lst
	  in
	  let t_list = List.rev t_list in
	  let t = TS.tuple_typ env.typ_env t_list in
	    env,t
	      

  let unify_leaf env k typ = 
    Log.note ("unify_leaf: " ^ k);
    match NS.find_local k env.ns with
      | None -> {env with ns = NS.add k typ env.ns}
      | Some typ' -> {env with typ_env = TS.unify_typ env.typ_env typ typ'}

  let rec ascend_to_nonmethod env = 
    if NS.at_root env.ns then env 
    else
      let t = NS.current_val env.ns in
	if TS.is_method_typ env.typ_env t
	then ascend_to_nonmethod {env with ns = NS.leave_scope env.ns}
	else env

  let type_method_return env t pos = 
      env (*
    let mt = 
      match NS.current_val env.ns with
	| MethodTyp mt -> mt
	| _ -> assert false
    in
      begin try ignore(RType.unify t mt.RType.ret_type); 
	with RType.Unification msg -> Log.err msg ~pos:pos
      end;
      env*)

  let rec type_lhs env lhs pos = 
    match lhs with
      | #identifier as id -> lookup_id_type env id
      | `Star `Unbound -> env, TS.fresh_typ env.typ_env
      | `Star _ -> assert false (*of [identifier | `Tuple of lhs list]*)
      | `Tuple lhs_l ->
	  let env, t_l = List.fold_left
	    (fun (env,ts) x -> 
	      let env,t = type_lhs env x pos in
		env, t::ts
	    ) (env,[]) lhs_l 
	  in
	  let tup_t = TS.tuple_typ env.typ_env (List.rev t_l) in
	    env, tup_t

  let type_assignment env lhs rhs_t pos = 
    env

  let type_formal env formal : TS.typ = 
    TS.fresh_typ env.typ_env

  let rec type_stmt env (stmt:Cfg.StmtRec.stmt) = 
    (*  Log.err "ping" (pos_of stmt);*)
    match stmt.snode with
      | Module (name_id,body) ->
	  let old_locals = env.locals in
	  let env = ascend_to_nonmethod env in
	  let scope,name = NS.scope_of_identifier_split name_id in
	  let full_scope = NS.absolute_scope (NS.append_scope scope name) env.ns in
	  let in_env = env in
	  let env = type_in_scope
	    (fun env _ ->
	      let mod_t = TS.module_typ env.typ_env full_scope in
	      let env = {env with ns=NS.add name mod_t env.ns; self=mod_t} 
	      in 
		type_in_scope 
		  (fun env _ ->
		     type_stmt (fresh_scope env) body
		  ) env () (NS.Relative_Scope [name])
	    ) env () scope
	  in {env with self=in_env.self;locals = old_locals}

      | Class(MetaClass _, _) -> 
	  Log.err "***skipping meta class***" ~pos:stmt.pos;
	  env

      | Class(NominalClass(name_expr,inh),body) ->
	  let old_locals = env.locals in
	  let env = ascend_to_nonmethod env in
	  let scope,name = NS.scope_of_identifier_split name_expr in
	  let full_scope = NS.absolute_scope (NS.append_scope scope name) env.ns in
	  let in_env = env in
	    (* first move to the scope where the class is defined *)
	  let env = type_in_scope
	    (fun env _ -> 
	      (* Add the class at that scope *)
	      let class_t = TS.class_typ env.typ_env full_scope in
	      let env = {env with ns=NS.add name class_t env.ns} in
		(* descend into the class *)
		type_in_scope
		  (fun env _ ->
		    let ns = match inh with
			(* If we don't directly inherit from anyone, we inherit from
			   Object *)
		      | None -> NS.add_link (NS.Absolute_Scope ["Object"]) env.ns 
		      | Some e -> 
			  try
			    let scope = NS.lookup_scope (NS.scope_of_identifier e) env.ns in
			      NS.add_link scope env.ns
			  with Not_found ->
			    Log.err ~pos:stmt.pos ("unable to find class to inherit from: " ^
						 (string_of_format format_id e));
			    env.ns
		    in
		    let env = {env with ns=ns; self=class_t} in
		      type_stmt (fresh_scope env) body
		  ) env () (NS.Relative_Scope [name])
	    ) env () scope
	  in
	    {env with self = in_env.self; locals = old_locals}

      | Method(name_expr,params,body) ->
	  let old_locals = env.locals in
	  let scope,name = match name_expr with
	    | Instance_Method msg -> 
		(NS.Relative_Scope []), (string_of_format format_msg msg)
	    | Singleton_Method(id1,msg) ->
		(*
		let env, t = lookup_id_type env id1 pos in
		let s = try RType.scope_of_typ t with
		  | Failure _ -> Log.err ~pos:pos "singleton scope lookup error"; 
		      NS.Relative_Scope []
		in
		  s, (string_of_format format_msg msg)
		*) assert false
	    | Class_Method(id1,msg) -> 
		NS.scope_of_identifier id1, (string_of_format format_msg msg)
	  in
	  let formal_typs = List.map (type_formal env) params in
	  let ret_typ = TS.fresh_typ env.typ_env in
	  let meth_t = TS.method_typ env.typ_env formal_typs ret_typ in
	  let () = Log.note (sprintf "adding method %s : %s at scope %s"
			       name (TS.string_of_typ meth_t) 
			       (NS.string_of_curs env.ns)) 
	  in

	  let env = unify_leaf env name meth_t in
	  let env = add_formals_to_env env params formal_typs stmt.pos in

	  let s = NS.append_scope scope name in
	  let env = type_in_scope type_stmt env body s in
	    {env with locals = old_locals}

      | ExnBlock(b) -> type_body b env stmt.pos (* TODO: new scope? *)

      | MethodCall(_,{mc_target = `ID_Self;
		      mc_msg = `ID_MethodName "include";
		      mc_args = [i];
		      mc_cb = None;
		     }) ->
	  begin try
	      let scope = NS.lookup_scope (NS.scope_of_identifier (id_of_expr i)) env.ns in
	      let ns = NS.add_link scope env.ns in
		{env with ns=ns}
	    with Not_found -> 
	      Log.err ~pos:stmt.pos ("unable to find module to include: " ^ 
				   (string_of_format format_expr i));
	      env
	  end

      | MethodCall(_,{mc_target = `ID_Self;
		      mc_msg = (`ID_MethodName "load"|`ID_MethodName "require");
		      mc_args = [inc];
		      mc_cb = None;
		     }) ->
	  begin match inc with
	    | `Lit_String id -> 
		Log.note (sprintf "saw require/load: %s" id) ~pos:stmt.pos;
		if StrSet.mem id env.already_typed then env
		else begin try
		    let stmt = File_loader.load_file env.loader id in
		    let env = {env with already_typed = StrSet.add id env.already_typed} in
		      type_in_scope type_stmt env stmt NS.root
		  with Not_found ->
		    Log.err ("unable to load require'd file: " ^ id) ~pos:stmt.pos;
		    env
		  end
	    | _ -> 
		Log.err (sprintf "require non-constant: %s"
			    (string_of_format format_expr inc)) ~pos:stmt.pos;
		env
	  end

      | MethodCall(_,{mc_target = `ID_Self;
		      (* attr_reader attr_writer attr_accessor *)
		      mc_msg = `ID_MethodName ("attr" | "attr_reader"
		      | "attr_writer" | "attr_accessor");
		      mc_args = args;
		      mc_cb = None;
		     }) ->
	  Log.err "**** handle attr! ****" ~pos:stmt.pos;
	  env

      | MethodCall(_,({mc_target = `ID_Self;
		       mc_msg = `ID_MethodName
	    ("public" | "private" | "protected" as access_name);
		       mc_cb = None;
		      } as mc)) ->
(*
	  let new_acc = access_of_string access_name in
	  let ns = 
	    List.fold_left (fun ns meth -> update_access meth new_acc ns)
	      env.ns mc.mc_args
	  in
	    {env with ns=ns}
*)
	  ignore(mc,access_name);
	  Log.err ~pos:stmt.pos "update access!";
	  env
	      
      | MethodCall(None, mc) -> 
	  let env, mc_ret = type_methodcall env mc stmt.pos in
	    env
      | MethodCall(Some lhs, mc) -> 
	  let env, mc_ret = type_methodcall env mc stmt.pos in
	    type_assignment env lhs mc_ret stmt.pos

      | While(g,body) ->type_stmt (type_stmt env g) body

      | If(g,t,f) ->
	  let env,_ = type_expr env g in
	  let env = type_stmt env t in
	    type_stmt env f

      | For(params,g,el) ->
	  Log.err "add params in For" ~pos:stmt.pos;
	  let env = type_stmt env g in
	    type_stmt env el

      (* These have their own local scope *)
      | Begin(el) | End(el) -> 
	  let env' = type_stmt (fresh_scope env) el in
	    {env' with locals = env.locals}
	      
      | Block(el) -> List.fold_left type_stmt env el

      | Case(c) ->
	  let env = 
	    List.fold_left
	      (fun env (el1,el2) ->
		let env = type_stmt env el1 in
		  type_stmt env el2
	      ) env c.case_whens
	  in begin match c.case_else with
	    | None -> env
	    | Some x -> type_stmt env x
	    end

      | Expression (`ID_Var(_, "public"))    -> {env with acc = Public}
      | Expression (`ID_Var(_, "protected")) -> {env with acc = Protected}
      | Expression (`ID_Var(_, "private"))   -> {env with acc = Private}

      | Assign(lhs, rhs) ->
	  Log.warn (sprintf "saw assign to %s = %s" 
		       (string_of_format format_lhs lhs)
		       (string_of_format format_tuple rhs));
	  let env, rhs_t = type_tuple_expr env rhs stmt.pos in
	    type_assignment env lhs rhs_t stmt.pos

      (*
	let env, lhs_t = lookup_type env lhs in
	let env, e_t = type_expr env e in
	let t' = Unify.union lhs_t e_t in
	update_type env lhs t'
      *)
	      
      | Expression(e) -> 
	  (* just check to see if we find the identifier *)
	  ignore(type_expr env e);
	  env

      | Return(None) -> type_method_return env (RType.class_typ "NilClass") stmt.pos
      | Return(Some e) -> 
	  let env, t = type_tuple_expr env e stmt.pos in
	    type_method_return env t stmt.pos

      | Yield(_,_)  -> 
	  Log.err "Handle yield!" ~pos:stmt.pos;
	  env

      | Alias(link, existing) ->
	  Log.err "handle alias" ~pos:stmt.pos;
	  env

  and type_body body env pos : env = 
    let env = type_stmt env body.exn_body in
    let env =
      List.fold_left
	(fun env resc ->
	  let env = 
	    List.fold_left
	      (fun env -> function
		| Rescue_Bind(e,b) -> 
		    let env,t = type_expr env e in
		    let name = string_of_id b in
		    let locals = StrMap.add name t env.locals in
		      {env with locals=locals}
		| Rescue_Expr e ->
		    let acc,_ = type_expr env e in acc
	      ) env resc.rescue_guards
	  in
	    type_stmt env resc.rescue_body
	) env body.exn_rescue
    in
    let env = do_opt ~none:env ~some:(type_stmt env) body.exn_ensure in
    let env = do_opt ~none:env ~some:(type_stmt env) body.exn_else in
      env

  and add_formals_to_env env flst alst pos = match flst,alst with
    | [],[] -> env
    | _::_, [] -> Log.warn "more formals than actuals" ~pos:pos;env
    | [], _::_ -> Log.warn "more actuals than formals" ~pos:pos;env
    | formal::formals, actual::actuals -> 
	let env = match formal with
	  | Formal_id(`Var_Local,s) -> 
	      {env with locals = StrMap.add s (TS.fresh_typ env.typ_env) env.locals}

	  | Formal_rest -> assert(formals=[]);env

	  | Formal_default(Formal_id(`Var_Local,s),v) -> 
	      Log.warn "fix default formal";
	      {env with locals = StrMap.add s (TS.fresh_typ env.typ_env) env.locals}

	  | Formal_default _ ->
	      Log.warn "handle default formal";
	      env

	  | Formal_star(`Var_Local,s) ->
	      let typ = TS.class_typ env.typ_env (NS.Absolute_Scope ["Array"]) in
	      let old = 
		try StrMap.find s env.locals
		with Not_found -> TS.fresh_typ env.typ_env
	      in
	      let env = 
		try {env with typ_env=TS.unify_typ env.typ_env typ old}
		with TypeSystem.Unification msg -> 
		  Log.err ~pos:pos msg;
		  env
	      in
		{env with locals = StrMap.add s typ env.locals}
	  | Formal_star _ -> assert false (* need to fix parser *)

	  | Formal_id _
	  | Formal_amp(_)
	  | Formal_tuple(_) -> 
	      Log.warn "handle more formals in add_formals_to_env";
	      env
	in
	  add_formals_to_env env formals actuals pos


  let type_cfg stmt = type_stmt empty_env stmt

  let solve_constraints env = ignore(TS.solve_constraints env.typ_env)

end (* Make functor *)
