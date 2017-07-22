
open Cfg
open Utils
open Cfg_printer.CodePrinter
open Config
 
module NS = Namespace

type env = {
  lex_scope : NS.scope list;
  curs : unit NS.cursor;
}


let special_case_yaml f = 
  let yml = "/yaml.rb" in
  let ylen = String.length yml in
  let len = String.length f in
    if len >= ylen then
      (* the only external dep of yaml is stringio.so, so we
         substitute that for the actual yaml file (and place the yaml
         stubs in base_types) *)
      if String.sub f (len-ylen) ylen = yml
      then Filename.concat conf.stub_dir "stringio.so.rb"
      else f
    else f

let valid_scope sc curs pos = 
  try ignore(NS.lookup_scope sc curs)
  with Not_found ->
    Log.fatal (Log.of_loc pos)
      "Unable to locate the scope %s"
      (NS.string_of_scope sc)

let load_file seen loader fname rl pos func = 
  try
    let fname = special_case_yaml fname in
    let no_ext = match rl with `Require -> false | `Load -> true in
    let full_name = File_loader.find_file ~no_ext loader fname in
      if StrSet.mem full_name seen then ()
      else func full_name (File_loader.load_file loader fname ~no_ext)
  with Not_found -> ()

let rec add_constant ns pos lhs = match lhs with
  | `ID_Var(`Var_Constant,name) ->
      NS.add name () ns
  | (`ID_Scope _
    | `ID_UScope _ as id) -> 
      let sc,str = NS.scope_of_identifier_split id in 
        valid_scope sc ns pos;
        NS.do_in_scope (NS.add str ()) sc ns
          
  | `ID_Var _ | `ID_Self | `ID_Nil | `ID_True | `ID_False -> ns

  | `Tuple lst -> 
      List.fold_left (fun ns lhs -> add_constant ns pos lhs) ns lst
  | `Star (#identifier as id) -> add_constant ns pos (id :> lhs)

let add_parent_link pos ns scope =
  try
    let abs_scope = NS.lookup_scope scope ns in
      NS.add_link abs_scope ns
  with Not_found ->
    Log.err "Scope resolution: unable to find parent to inherit from: %s" 
      (NS.string_of_scope scope) ~ctx:(Log.of_loc pos);
    ns

let add_includes pos ns incs = 
  List.fold_left
    (fun ns e -> match e with
       | (`ID_Var(`Var_Constant,_) | `ID_Scope _ | `ID_UScope _ ) as id -> 
           let sc = NS.scope_of_identifier id in
             add_parent_link pos ns sc
       | _ as arg -> 
           Log.err ~ctx:(Log.of_loc pos)
             "unable to determine module to inherit from: %a"
             format_star_expr arg;
           ns
    ) ns incs

let id_of_abs_scope sc = 
  let rec helper acc = function
    | [] -> acc
    | x::xs -> 
        assert(is_capital x);
        helper (`ID_Scope(acc,x)) xs
  in match sc with
    | NS.Relative_Scope _ -> 
        Log.fatal Log.empty "relative scope passed to id_of_abs_scope" 
    | NS.Absolute_Scope [] -> assert false
    | NS.Absolute_Scope (x::tl) -> 
        assert(is_capital x);
        let root = `ID_UScope x in
          helper root tl

let lexical_lookup pos curs lex_scopes str = 
  match NS.find_local str curs with
    | Some () -> NS.current_scope (NS.enter_scope str curs)
    | None -> 
        try_each
          (fun scope -> 
             let curs' = NS.move_to_scope scope curs in
               match NS.find_local str curs' with
                 | Some () -> 
                     NS.current_scope (NS.enter_scope str curs')
                 | None -> raise Not_found
          ) lex_scopes

let find_scope pos env sc = 
  try begin match sc with
    | NS.Absolute_Scope _ -> Some (id_of_abs_scope (NS.lookup_scope sc env.curs))
    | NS.Relative_Scope [] -> assert false
    | NS.Relative_Scope (x::xs) ->
        let lscope = lexical_lookup pos env.curs env.lex_scope x in
        let full_scope = List.fold_left NS.append_scope lscope xs in
        let abs_scope = NS.lookup_scope full_scope env.curs in
          Some (id_of_abs_scope abs_scope)
  end with Not_found -> 
    Log.err ~ctx:(Log.of_loc pos)
      "Unable to statically locate scope %s in namespace hierarchy at %s" 
      (NS.string_of_scope sc) (NS.string_of_scope (NS.current_scope env.curs));
    None

class type scope_visitor = 
object
  inherit cfg_visitor
  method get_cursor : unit NS.cursor
  method get_entry : (string,unit) Hashtbl.t
end

open Visitor

class build_scope_visitor loader c : scope_visitor = 
object(self)
  inherit default_visitor as super
    
  val mutable curs = c
  method get_cursor = curs
  val entry_tests = Hashtbl.create 127
  method get_entry = entry_tests
  val mutable cur_file = "<unknown>"
  val mutable seen_files = StrSet.empty
  val mutable epos = None

  method private load_file pos arg rl = match arg with
    | `Lit_String fname ->
        load_file seen_files loader fname rl pos begin fun name stmt -> 
          seen_files <- StrSet.add name seen_files;
          let cursor =
            NS.do_in_scope
              (fun cursor ->
                 let old_file = cur_file in
                   curs <- cursor;
                   cur_file <- name;
                   ignore(visit_stmt (self:>cfg_visitor) stmt);
                     cur_file <- old_file;
                     curs
              ) NS.root curs
          in
            curs <- cursor;
        end
    | e -> 
        Log.err ~ctx:(Log.of_loc pos) "require/load non-constant: %a"
          format_star_expr e

  method visit_stmt stmt = 
    epos <- Some stmt.pos;
    match stmt.snode with
      | MethodCall(lhs_o,({ mc_msg = `ID_MethodName ("require"|"load" as m);
                            mc_args = file::_;
                            mc_cb = None}))
      | MethodCall(lhs_o,({ mc_msg = `ID_MethodName ("safe_require"|"safe_load" as m);
                            mc_args = _::file::_;
                            mc_cb = None})) ->
          let rl = if m = "safe_require" || m = "require" then `Require else `Load in
            self#load_file stmt.pos file rl;
            DoChildren

      | MethodCall(Some (`ID_Var(`Var_Local,var)),
                   {mc_target = Some (`Lit_String s);
                    mc_msg = `ID_Operator Op_EQ;
                    mc_args = [`ID_Var(`Var_Builtin,"$0")];
                    mc_cb = None}) when conf.single_entry ->
          Hashtbl.add entry_tests var ();
          SkipChildren
              
    | MethodCall(_,{mc_target = None;mc_msg = `ID_MethodName "include";
                    mc_args = args;mc_cb = None}) -> 
        curs <- add_includes stmt.pos curs args;
        DoChildren

    | MethodCall(_, ({mc_msg = `ID_MethodName "const_set";
		      mc_args = [(`Lit_Atom lhs|`Lit_String lhs);_]} as mc)) -> 
        let const = match mc.mc_target with
          | None -> `ID_Var(`Var_Constant,lhs)
          | Some (#identifier as e) -> `ID_Scope(e,lhs)
          | _ -> Log.fatal Log.empty "fix the missing const_get case"
        in
          curs <- add_constant curs stmt.pos const;
          DoChildren
          
    | If(`ID_Var(`Var_Local,var), t, f) ->
        if conf.single_entry && Hashtbl.mem entry_tests var 
        then SkipChildren
        else DoChildren

    | Class(lhs_o, NominalClass(id,inh), body) ->
        let sc,str = NS.scope_of_identifier_split id in
        let () = valid_scope sc curs stmt.pos in
        let cursor = 
          NS.do_in_scope
            (fun cursor -> 
               let outer_cursor = NS.add str () cursor in
               let inner_sc = NS.Relative_Scope [str] in
                 NS.do_in_scope
                   (fun cursor -> 
                      let parent = match inh with
                        | None -> NS.Absolute_Scope ["Object"]
                        | Some `ID_Self -> NS.current_scope outer_cursor
                        | Some e -> NS.scope_of_identifier e
                      in
                        curs <- add_parent_link stmt.pos cursor parent;
                        ignore(visit_stmt (self:>cfg_visitor) body);
                        curs
                   ) inner_sc outer_cursor
            ) sc curs
        in
          curs <- cursor;
          do_opt ~none:() ~some:(fun l -> ignore(visit_lhs (self:>cfg_visitor) l)) lhs_o;
          SkipChildren

    | Module(lhs_o, id, body) ->
        let sc,str = NS.scope_of_identifier_split id in
        let () = valid_scope sc curs stmt.pos in
        let cursor = NS.do_in_scope
          (fun cursor -> 
             let cursor = NS.add str () cursor in
             let inner_sc = NS.Relative_Scope [str] in
               NS.do_in_scope
                 (fun cursor -> 
                    let parent = NS.Absolute_Scope ["Module"] in
                      curs <- add_parent_link stmt.pos cursor parent;
                      ignore(visit_stmt (self:>cfg_visitor) body);
                    curs
                 ) inner_sc cursor
          ) sc curs
        in
          curs <- cursor;
          do_opt ~none:() ~some:(fun l -> ignore(visit_lhs (self:>cfg_visitor) l)) lhs_o;
          SkipChildren

    | Defined(i,s) -> 
        ignore(visit_id (self:>cfg_visitor) i);
        SkipChildren

    | Assign((`ID_Var(`Var_Constant,_)| `ID_Scope _ | `ID_UScope _ as l),
             (`ID_Var(`Var_Constant,_)| `ID_Scope _ | `ID_UScope _ as r)) ->
        let l_sc = NS.scope_of_identifier l in
        let r_sc = NS.scope_of_identifier r in
        let () = valid_scope r_sc curs stmt.pos in
        let cursor = add_constant curs stmt.pos l in
        let cursor = NS.do_in_scope
          (fun cursor ->
             add_parent_link stmt.pos cursor r_sc
          ) l_sc cursor
        in
          curs <- cursor;
          SkipChildren

    | _ -> DoChildren

  method visit_lhs lhs = 
    match epos with None -> assert false | Some pos ->
      curs <- add_constant curs pos lhs;
      DoChildren
end

class update_scope_visitor loader entry_tests theenv : cfg_visitor = 
object(self)
  inherit default_visitor as super

  val mutable env = theenv
  val mutable pos = Lexing.dummy_pos
  val mutable seen_files = StrSet.empty

  method private in_env f a new_env = 
    let old_env = env in
      env <- new_env;
      let ret = f a in
        env <- old_env;
        ret

  method private update_file pos env arg rl = match arg with
    | `Lit_String id -> 
        load_file seen_files loader id rl pos begin fun name stmt -> 
          seen_files <- StrSet.add name seen_files;
          let curs = NS.move_to_scope NS.root env.curs in
          let env' = {lex_scope=[NS.root];curs=curs} in
          let stmt' = self#in_env (visit_stmt (self:>cfg_visitor)) stmt env' in
            File_loader.update_file loader name stmt';
        end

    | _ -> () (* already complained *)

  method visit_stmt stmt = 
    pos <- stmt.pos;
    match stmt.snode with

      | MethodCall(lhs_o,({ mc_msg = `ID_MethodName ("require"|"load" as m);
                            mc_args = file::_;
                            mc_cb = None}))
      | MethodCall(lhs_o,({ mc_msg = `ID_MethodName ("safe_require"|"safe_load" as m);
                            mc_args = _::file::_;
                            mc_cb = None})) ->
          let rl = if m = "safe_require" then `Require else `Load in
            self#update_file stmt.pos env file rl;
            DoChildren

      | If(`ID_Var(`Var_Local,var),t,f) ->
          if conf.single_entry && Hashtbl.mem entry_tests var 
          then SkipChildren
          else DoChildren

      | Class(lhs_o,NominalClass(id,inh),body) ->
          let sc = NS.scope_of_identifier id in
          let cls_curs = NS.move_to_scope sc env.curs in
          let env' = {lex_scope=(NS.current_scope cls_curs)::env.lex_scope; 
                      curs=cls_curs} 
          in
            (*let annot' = map_opt_preserve (visit_annot_class (self:>cfg_visitor)) annot in
            *)
            
          let id' = visit_id (self:>cfg_visitor) id in
          let inh' = map_opt_preserve (visit_id (self:>cfg_visitor))inh in
          let lhs_o' = map_opt_preserve (visit_lhs (self:>cfg_visitor)) lhs_o in
          let body' = self#in_env (visit_stmt (self:>cfg_visitor)) body env' in
            if lhs_o==lhs_o' && id==id' && inh == inh' && body==body'
            then SkipChildren
            else 
              let ck' = NominalClass(id',inh') in
              let s' = update_stmt stmt (Class(lhs_o',ck',body')) in
                ChangeTo s'

      | Module(lhs_o,id,body) ->
          let id' = visit_id (self:>cfg_visitor) id in
          let lhs_o' = map_opt_preserve (visit_lhs (self:>cfg_visitor)) lhs_o in
          let sc = NS.scope_of_identifier id in
          let mod_curs = NS.move_to_scope sc env.curs in
          let env' = {lex_scope=(NS.current_scope mod_curs)::env.lex_scope;
                      curs = mod_curs } 
          in
          let body' = self#in_env (visit_stmt (self:>cfg_visitor)) body env' in
            if lhs_o==lhs_o' && id==id' && body==body'
            then SkipChildren
            else ChangeTo (update_stmt stmt (Module(lhs_o',id',body')))

      | Method(Singleton_Method(`ID_Self as id,msg),args,body) ->
          let curs = 
            if NS.at_root env.curs then env.curs 
            else NS.leave_scope env.curs 
          in
          let env' = {lex_scope=env.lex_scope; curs=curs} in
          let id' = visit_id (self:>cfg_visitor) id in
          let msg' = visit_msg_id (self:>cfg_visitor) msg in
          let args' = 
            map_preserve List.map (visit_method_param (self:>cfg_visitor)) args 
          in
          let body' = self#in_env (visit_stmt (self:>cfg_visitor)) body env' in
            if id==id' && msg==msg' && args==args' && body==body'
            then SkipChildren
            else 
              let sm = Singleton_Method(id',msg') in
              let m = update_stmt stmt (Method(sm,args',body')) in
                ChangeTo m

      | Defined(i,s) -> 
          let i' = visit_id (self:>cfg_visitor) i in 
            if i!= i' then ChangeTo(update_stmt stmt (Defined(i',s)))
            else SkipChildren

      | _ -> DoChildren

  method visit_id id = match id with
    | `ID_Var(`Var_Constant,s) -> 
        begin match find_scope pos env (NS.Relative_Scope [s]) with
          | None -> DoChildren
          | Some x -> ChangeTo x
        end
          
    | `ID_Var _ -> DoChildren
        
    | `ID_UScope _
    | `ID_Scope _ -> 
        begin match find_scope pos env (NS.scope_of_identifier id) with
          | None -> DoChildren | Some x -> ChangeTo x
        end
          
    | `ID_Self | `ID_Nil | `ID_True | `ID_False -> DoChildren

end

type t = {
  loader : File_loader.t;
  builder : build_scope_visitor;
}

let create loader = 
  let ns = NS.to_cursor NS.empty in
  let ns = NS.add "Object" () ns in
  let ns = NS.add "Module" () ns in
  let ns = NS.add_link (NS.Absolute_Scope ["Object"]) ns in
  let builder = new build_scope_visitor loader ns in
    {loader = loader; builder=builder}

let resolve_scopes t stmt =   
  let _ = visit_stmt (t.builder:>cfg_visitor) stmt in
  let ns = t.builder#get_cursor in
  let env = {lex_scope = [NS.root]; curs = ns} in
  let updater = new update_scope_visitor t.loader t.builder#get_entry env in
    visit_stmt updater stmt 

