open Ast
open Ast_printer
open Test_helper
open Parse_helper
open OUnit


let builtin_ops = 
  [Op_UBang; Op_UNot; Op_UMinus; Op_UPlus; Op_UTilde]

let test_parse_all_builtins () = 
  let num = 1 in
    List.map 
      (fun op ->
	 let op_str = str_uop op in
	 let ast = [E_Unary(op, ast_num num,dp)] in
	 let str = Printf.sprintf "%s %d" op_str num in
	 let func = fun () ->
	   let ast' = parse_string str in
	     assert_ast_equal ast ast'
	 in
	   ("test builtin op " ^ op_str) >:: func
      ) builtin_ops



let suite = "Unary Operator Parse Tests" >:::
  test_parse_all_builtins ()
