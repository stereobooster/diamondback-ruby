
open Ast
open Test_helper
open OUnit

(*
  | E_Ternary of expr * expr * expr
  | E_While of expr * expr list
  | E_For of string * expr * expr list
  | E_Return of expr option
  | E_If of expr * expr list
  | E_Unless of expr * expr list
*)


let tests = [
  ("for", 
   [E_For([Formal_id (ast_id "i");Formal_rest],
          E_Binop(ast_num 1, Op_DOT2, ast_num 2,dp),
	  [ast_id "i"],dp)],
   "for i, in 1..2 do i end");

  ("for", 
   [E_For([Formal_id (ast_id "i");Formal_rest],
          E_Binop(ast_num 1, Op_DOT3, ast_id "n",dp),
	  [ast_id "i"],dp)],
   "for i, in 1...n do i end");

];;

let suite = "Control Operator Parse Tests" >:::
  List.map simple_test tests
