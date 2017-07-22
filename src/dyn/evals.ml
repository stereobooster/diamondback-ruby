open Config
open Printf
open Dynamic
open Yaml
open Dyn_builder

module Domain = YString
module CoDomain = YString

let name = "eval"

let instrument_ast a = a

let watch_f meth = 
  {src_msg = `ID_MethodName meth;
   dst_target = Cfg.Abbr.access_path ["DRuby";"Profile";"Eval"];
   dst_msg = `ID_MethodName "record_eval";
  }

let instrument_cfg cfg = 
  let watched = [
    watch_f "eval";
    watch_f "instance_eval";
    watch_f "class_eval";
    watch_f "module_eval"
  ]
  in
    Cfg.visit_stmt (new watch_visitor watched) cfg


open Ast

(** finds out which eval the given msg is referring to *)
let rec eval_mname_of_msg = function
  | E_Identifier(ID_Lowercase, s, pos) -> 
      if s = "eval" || s = "instance_eval" || s = "class_eval" ||
        s = "module_eval" then Some(s)
      else None
  | E_Binop(expr1, binary_op, expr2, pos) ->
      eval_mname_of_msg expr2 (* keep following the rhs of the dot op *)
  | _ -> None

(** finds recv of the given expression *)
let recv_of_msg = function
  | E_Binop(expr1, Op_DOT, expr2, pos) -> expr1
  | _ -> E_Empty

module A = Ast.Abbr
let raise_syntax_error pos = 
  [A.mcall (A.ident "raise" pos) [A.ident "SyntaxError" pos] pos]

let deep_transform_ast yaml_map flist ast =
  (* generates a new method call to an eval with a new block *)
  let rec gen_eval_mcall recv pos eval_mname code = 
    let m_id = A.ident eval_mname pos in
    let meth = 
      if recv == E_Empty then m_id
      else E_Binop(recv, Op_DOT, m_id, pos) 
    in
    let ast = 
      try Parse_helper.parse_string code 
        (* if the code fails to parse, we insert a SyntaxError
           exception instead, since this is a recoverable error in Ruby
        *)
      with e -> raise_syntax_error pos
    in
      (* handle nested dynamic features! *)
    let ast = Ast.mod_ast (set_pos pos) ast in (* sets pos to actual pos *)
    let ast = List.fold_left (fun a f -> f a) ast flist in 
      (* generic eval cannot take a block; just spit out the code by itself *)
      if eval_mname = "eval" 
      then E_Block(ast, pos) 
      else A.mcall meth [] ~cb:(A.cb ast pos) pos
  in

  (* replaces the eval with a conventional eval (w/ block arg) *)
  let rec replace_eval recv pos eval_mname eval_margs = 
    let filename = File_loader.normalize_filename pos.Lexing.pos_fname in
    let lineno = pos.Lexing.pos_lnum in
    let matched_value = Some (yaml_map eval_mname pos) in
    let rec gen_whens = function
      | [] -> []
      | h::t -> 
          let expr1 = A.single_str h pos in
          let expr2 = (gen_eval_mcall recv pos eval_mname h) in
            ([expr1], [expr2])::(gen_whens t)
    in
      (* $stderr.puts("...", eval_margs[0]) *) 
      (* XXX: Not sure whether we should return nil afterwards *) 
    let gen_else () = 
      let m_id = A.ident "puts" pos in
      let msg = E_Binop(E_Identifier(ID_Global, "stderr", pos), Op_DOT,
                        m_id, pos) 
      in
      let errmsg = sprintf "\"%%s\" has previously not been seen at %s:%i"
        filename lineno
      in
      let margs = [A.single_str errmsg pos; List.hd eval_margs]
      in
        A.mcall msg margs pos
    in
      match matched_value with
        | Some(clist) -> 
            (* List.iter (fun c -> print_endline c) clist; *)
            (match clist with
               | [] -> None
               | [c] -> Some(gen_eval_mcall recv pos eval_mname c)
               | l -> 
                   let lst = gen_whens l in
                   let guard = 
                     try List.hd eval_margs with _ ->
                       Log.fatal Log.empty "invalid eval statement"
                   in
                   let case_b = {case_guard=guard; case_whens=lst;
                                 case_else=[gen_else()]} 
                   in
                     Some(E_Case(case_b, pos))
            )
        | None -> None              
  in

  let rec transform_evals_expr = function
    | E_MethodCall(msg, el, None, pos) as expr -> 
        (match el with
           | (E_Unary(Op_UAmper, _, _)::_) -> expr (* block arg; quit *)
           | _ -> (match eval_mname_of_msg msg with
                     | None -> expr (* no eval; quit *)
                     | Some(eval_name) ->
                         let res = replace_eval (recv_of_msg msg) pos eval_name el in
                           Utils.default_opt expr res
                             
                  )
        )
    | expr -> expr
  in
    mod_ast transform_evals_expr ast
      
let transform_ast map ast = deep_transform_ast map [] ast
let transform_cfg hsh s = s



