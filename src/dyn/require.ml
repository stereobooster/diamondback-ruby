
open Yaml
open Ast
open Utils
open Config


module Domain = YString
module CoDomain = 
  Y4Tuple
    (YBool)
    (YString)
    (YOr(YBool)(YString))
    (YList(YString)) 

module Data = YMap(Y3Tuple(YString)(YInt)(Domain))(YList(CoDomain))  
type t = Data.t

let name = "require"

let instrument_ast ast = ast
let instrument_cfg cfg = cfg


let load_runtime_data func = 
  let map = Data.of_yaml (func "require") in
    (*Data.Map.iter
      (fun (f,l,_) lst ->
         Printf.eprintf "%s:%d - " f l;
         List.iter (fun (_,s,b,_) -> 
                      let ret = match b with
                        | `A b -> Printf.sprintf "%b" b
                        | `B s -> s
                      in
                        Printf.eprintf "%s ret: %s" s ret
                   ) lst;
         Printf.eprintf "\n%!"
      ) map;*)
    map
      
let profile_libs = ["require"]

let all_required_files t = 
  Data.Map.fold
    (fun (srcfile,srcline,_) lst acc ->
       List.fold_left 
         (fun acc (is_req,file,really_load,libdirs) ->
            match really_load with
                (* file was successfully loaded at runtime *)
              | `A true -> begin try
                  let fqname = File_loader.find_in_libpath ~no_ext:(not is_req) file libdirs in
                    (* skip .so files *)
                    if File_loader.is_native fqname then acc
                    else (fqname,File_loader.normalize_filename fqname)::acc
                with Not_found ->
                  Log.warn ~ctx:Log.empty "didn't find file %s in paths %s"
                    file (list_to_string id libdirs);
                  acc
                end 
              | `A false -> acc (* loaded elsewhere *)
              | `B exn -> acc (* raised an exception at runtime *)
         ) acc lst
    ) t []

let dr_runtime s pos = 
  let module A = Ast.Abbr in
  let cls = A.scoped_ident ["DRuby";"Profile";"Runtime"] pos in
  let meth = E_Identifier(ID_Lowercase, s, pos) in
    E_Binop(cls,Op_DOT,meth,pos)

(** generate actual ast node for require/load *)
let gen_require req_or_load ~orig name pos = 
  let with_ext = match extension orig, extension name with
    | Some(".rb"|".so"),_ -> orig (* already has prefix *)
    | _, None -> orig (* require fails *)
    | _, Some e -> orig ^ e
  in
  let orig = E_Literal(Lit_String(String_Single with_ext), pos) in
  let arg = E_Literal(Lit_String(String_Single name), pos) in
    match req_or_load with
      | `Require -> 
          if name = "" 
          then E_MethodCall(dr_runtime "exn_require" pos, [orig], None, pos) 
          else E_MethodCall(dr_runtime "safe_require" pos, [orig;arg], None, pos) 
  
      | `Load wrap -> 
          if name = "" 
          then E_MethodCall(dr_runtime "exn_load" pos, orig::wrap, None, pos)
          else E_MethodCall(dr_runtime "safe_load" pos, orig::arg::wrap, None, pos)

(** generates a call to DRuby::Profile::Runtime.dead_require() *)
let dead_require req_or_load star_args req_mcall pos = 
  let scoped = dr_runtime "dead_require" pos in
  let mname = match req_or_load with
    | `Require -> "require"
    | `Load _ -> "load"
  in
  let recv = E_Literal(Lit_Self, pos) in
  let args = [recv; E_Literal(Lit_String (String_Single mname), pos)] in
  let block = E_CodeBlock(true, None, [(*FIXME:req_mcall*)], pos) in
    E_MethodCall(scoped, args @ star_args, Some(block), pos)

(** generates a case statement for require if it is dynamic (dead_require if
    not has been seen before *)
let gen_require_switch req_or_load arg filemap req_mcall pos = 
  if StrMap.is_empty filemap then begin
    dead_require req_or_load [arg] req_mcall pos
  end else match arg with
    | E_Literal(Lit_String(String_Double [StrChars orig] | String_Single orig), pos) ->
        let req = StrMap.fold 
          (fun orig mapped_set acc ->
             StrSet.fold 
               (fun mapped acc ->
                  (gen_require req_or_load ~orig mapped pos)::acc
               ) mapped_set acc
          ) filemap []
        in begin match req with
          | [] -> assert false
          | [x] -> x
          | lst -> 
              Log.fatal (Log.of_loc pos) "require literal had multiple files? %s"
                (list_to_string (Ast_printer.string_of_expr) req)
          end
    | _ -> 
        let () = (* mining *)
          let mname = if req_or_load = `Require then "require" else "load" in
            if conf.mine_dyn_feats then
              Mining.mine  
                pos.Lexing.pos_fname
                pos.Lexing.pos_lnum 
                mname
        in
        let whens = StrMap.fold 
          (fun orig mapped_set acc ->
             StrSet.fold 
               (fun mapped acc ->
                  let g1 = [E_Literal(Lit_String(String_Single orig), pos)] in
                  let g2 = [E_Literal(Lit_String(String_Single mapped), pos)] in
                  let req = [gen_require req_or_load ~orig mapped pos] in
                    (g1,req)::(g2,req)::acc
               ) mapped_set acc
          ) filemap []
        in
        let else' = [dead_require req_or_load [arg] req_mcall pos] in
          E_Case({case_guard=arg;case_whens=whens;case_else=else'},pos)

let lookup_file tmp_tbl is_req file is_load libdirs pos = 
  match is_load with
    | `A _ -> (* file existed at runtime *)
        begin  
          let f = 
            try File_loader.find_in_libpath ~no_ext:(not is_req) file libdirs 
            with Not_found -> Log.fatal Log.empty "didn't find %s in libpath %s"
              file (list_to_string id libdirs)
          in
          let f = File_loader.normalize_filename f in
            if File_loader.is_native f 
            then f, f
            else try f, Hashtbl.find tmp_tbl (File_loader.normalize_filename f)
            with Not_found -> 
              Log.fatal (Log.of_loc pos)
                "didn't find %s(%s) in tmp_tbl" file f
        end
          
    | `B exn -> file, "" (* require raised an exception at runtime *)

let last_pos e = 
  let pos = ref (Ast.pos_of e) in
  let f e = 
    let pos' = Ast.pos_of e in
      if pos'.Lexing.pos_lnum > (!pos).Lexing.pos_lnum
      then pos := pos';
      e
  in ignore(Ast.mod_ast f [e]); !pos

(* transforms requires into ones with string literals 
 * ex) require some_list => require 'x'; require 'y'
 * FIXME: receiver of the call must be checked
 *)
let rec instrument_expr tmp_tbl (req_map:t) expr = 
  (* finds the corresponding require arguments *)
  let rec replace_require req_or_load arg pos =
    let filename = File_loader.normalize_filename pos.Lexing.pos_fname in
    let lineno = pos.Lexing.pos_lnum in
    let mname = match req_or_load with `Require -> "require" | `Load _ -> "load" in
    let finfo = 
      try Data.Map.find (filename, lineno,mname) req_map
      with Not_found -> (
        Log.warn ~ctx:(Log.of_loc pos) "replace_requires(%s,%d)" 
          filename lineno; [])
    in 
      if finfo = [] then begin
        (*Printf.eprintf "\n\n***finfo is empty for %s:%d\n" filename lineno;
          Data.Map.iter (fun (f,l) v -> Printf.eprintf "key: %s:%d\n" f l) req_map;*)
        
      end;
      (* there is a weird corner case that we need to handle.  If a
         require statement is executed twice with the same argument,
         but one time it raises and exception and the other time it
         successfully requires the file.  This can happen if the
         program has instrumented Kernel#require to try additional
         load paths.  In this case, we throw away the initial failed
         attempts and keep only the successful translation. *)
      let split_finfo = List.fold_left
        (fun (a,b) ((is_req,file,really_load,libdirs) as tup) -> 
           match really_load with
             | `A _ -> tup::a, b
             | `B _ -> a, tup::b
        ) ([],[]) finfo 
      in
      let finfo = match split_finfo with
        | [], [] -> []
        | [], x -> x
        | x, [] -> x
        | succ,exn -> succ (* keep eventually successful require *)
      in
      let file_map = List.fold_left 
        (fun acc (is_req,file,really_load,libdirs) -> 
           let fqfn, newv = lookup_file tmp_tbl is_req file really_load libdirs pos in
           let set = try StrMap.find file acc with Not_found -> StrSet.empty in
           let set = StrSet.add newv set in
             StrMap.add file set acc
        ) StrMap.empty finfo
      in gen_require_switch req_or_load arg file_map expr pos
  in match expr with
    | E_MethodCall(E_Binop(E_Identifier(ID_Uppercase,"Kernel",_),
                           Op_DOT,
                           E_Identifier(ID_Lowercase, "require", _),_),
                   [arg], None, _)
    | E_MethodCall(E_Identifier(ID_Lowercase, "require", _), [arg], None, _) ->
        replace_require `Require arg (last_pos expr)

    | E_MethodCall(E_Binop(E_Identifier(ID_Uppercase,"Kernel",_),
                           Op_DOT,
                           E_Identifier(ID_Lowercase, "load", _),_),
                   arg::wrap, None, pos)
    | E_MethodCall(E_Identifier(ID_Lowercase, "load", _), arg::wrap, None, pos) ->
        replace_require (`Load wrap) arg (last_pos expr)
    | _ -> expr

let modify_requires tbl (ri:t) ast = 
  try Ast.mod_ast (instrument_expr tbl ri) ast
  with e -> Log.fatal Log.empty "mod_ast: %s" (Printexc.to_string e)





(*
open Cfg
module C = Cfg.Abbr
open Dyn_builder

module Base = struct
  module Domain = Domain
  module Codomain = YPair(YString)(YOption(YString))
  let preamble = []
  let when_guard (s1,s2) = C.str s1
end

let safe_require lhs s1 s2 pos = 
  C.mcall ?lhs (`ID_MethodName "safe_require") [C.str s1;C.str s2] pos

let exn_require lhs s1 pos = 
  C.mcall ?lhs (`ID_MethodName "exn_require") [C.str s1] pos

class require_visitor yaml_map = 
object(self)
  inherit default_visitor
  method visit_stmt s = match s.snode with
    | Require(lhs,(#expr as file)::rest,`Require) -> 
        let module M = struct
          include Base
          let case_guard = file
          let else_body = dead_method "require" [(file :> method_arg_expr)] s.pos
          let when_body pair = match pair with
            | s1, None -> exn_require lhs s1 s.pos
            | s1, Some s2 -> safe_require lhs s1 s2 s.pos
        end in let module T = CaseTransformer(M) in
          T.transform yaml_map "require" s.pos
    | _ -> Visitor.SkipChildren
end
*)
let transform_ast ym ast = ast
let transform_cfg yaml_map cfg = cfg

