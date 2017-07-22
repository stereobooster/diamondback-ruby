

open Ast
open Ast_printer
open OUnit
open Printf
open Parse_helper

let assert_ast_equal = assert_equal ~cmp:equal_ast ~printer:string_of_ast

let dp = Lexing.dummy_pos

let ast_num i = E_Literal(Lit_FixNum i, dp)
let ast_float f = E_Literal(Lit_Float(string_of_float f, f), dp)

let ltrue = E_Literal(Lit_True,dp)
let lfalse = E_Literal(Lit_False,dp)
let lself = E_Literal(Lit_Self,dp)
let lnil = E_Literal(Lit_Nil,dp)

let ast_id x =
  let len = String.length x in
    if len = 0 then
      E_Identifier(ID_Uppercase, "", dp)
    else
      let kind = match x.[0] with
	| 'a'..'z' | '_' -> ID_Lowercase
	| '@' -> if x.[1] = '@' then ID_Class else ID_Instance
	| '$' -> ID_Global
	| 'A'..'Z' -> ID_Uppercase
	| _ -> raise (Invalid_argument "ast_id")
      in
	if x.[len-1] = '=' then
	  E_Identifier(ID_Assign kind, String.sub x 0 (len-1), dp)
	else      
	  E_Identifier(kind, x, dp)

let ast_str kind = E_Literal((Lit_String kind),dp)
let sing_str s = ast_str (String_Single s)
let doub_str s = ast_str (String_Double [StrChars s])
let tick_str s = ast_str (String_Tick [StrChars s])
let ast_re s m = E_Literal(Lit_Regexp([StrChars s],m),dp)
let ast_atom s = E_Literal(Lit_Atom([StrChars s]),dp)

let simple_test (desc,ast,code) = 
  let d = desc ^ ": "  ^ code in
    d >:: (fun () -> 
	     let ast' = parse_string code in
	     let _ = Cfg_refactor.refactor_ast ast' in
	       assert_ast_equal ast ast'
	  )

let simple_test_ignore (desc,ast,code) = 
  let d = desc ^ ": "  ^ code in
    d >:: (fun () -> Printf.eprintf "SKIPPING TEST: %s\n" desc
	     (*assert_ast_equal ast (parse_string code)*))
  
let just_body es = 
  {body_exprs = es;
   rescue_exprs = [];
   ensure_expr = [];
   else_expr = [];
  }
  
