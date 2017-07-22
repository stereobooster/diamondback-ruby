open Ast
open Ast_printer
open Test_helper
open Parse_helper
open OUnit


let builtin_ops = 
  [ Op_ASSIGN; Op_PLUS; Op_MINUS; Op_TIMES; Op_REM; Op_DIV; Op_CMP;
    Op_EQ; Op_EQQ; Op_NEQ; Op_GEQ; Op_LEQ; Op_AND; Op_OR; Op_BAND;
    Op_BOR; Op_MATCH; Op_NMATCH; Op_XOR; Op_POW; Op_kAND; Op_kOR;
    Op_ASSOC;
    Op_OP_ASGN Op_PLUS;
  ]


let test_parse_all_builtins () = 
  let id1 = "x" in
  let id2 = "y" in
    List.map 
      (fun op ->
	 let op_str = str_binop op in
	 let ast = [E_Binop(ast_id id1,Op_ASSIGN,lnil,dp);
                    E_Binop(ast_id id1, op, ast_id id2,dp)] 
         in
	 let str = Printf.sprintf "%s=nil;%s %s %s" id1 id1 op_str id2 in
	 let func = fun () ->
	   let ast' = parse_string str in
	     assert_ast_equal ast ast'
	 in
	   ("binary op " ^ op_str) >:: func
      ) builtin_ops

let tests = [
  ("range", [E_Binop (ast_num 1, Op_DOT2, ast_num 3,dp)], "1..3");
  ("range", [E_Binop(ast_float 1.2, Op_DOT2,ast_float 3.4,dp)], "1.2..3.4");
  ("range", [E_Binop (ast_num 1, Op_DOT3, ast_num 3,dp)], "1...3");
  ("range", [E_Binop(ast_float 1.2, Op_DOT3,ast_float 3.4,dp)], "1.2...3.4");
  ("assign",
   [E_Binop(ast_id "x", Op_ASSIGN, E_Binop(ast_id "y", Op_ASSIGN, ast_id "z",dp),dp)],
   "x = y = z");
  ("aset", [E_Binop(
      E_MethodCall(E_Binop(E_Block([ast_id "x"],dp), 
			  Op_DOT, 
			  E_Operator(Op_AREF,dp),
			  dp),
		  [ast_num 1],None,dp),
      Op_ASSIGN,
      ast_id "y",dp)],
   "(x)[1] = y");

  ("^=",
   [E_Binop(ast_id "x", Op_OP_ASGN Op_XOR, ast_id "y",dp)],
   "x ^= y");

  ("multline multiassign",
   [E_Binop(E_Tuple([ast_id "x"; ast_id "y"],dp),
	    Op_ASSIGN,
	    E_Tuple([ast_num 1; ast_num 2],dp),dp)],
   "x, y =\n 1, 2");

  ("multiassign glob",
   [E_Binop(E_Tuple([ast_id "x";E_UOperator(Op_UStar,dp)],dp),
	    Op_ASSIGN,
	    ast_id "y",dp)],
   "x,* = y");

  ("/=",
  [E_Binop(ast_id "x",Op_ASSIGN,ast_num 2,dp);
   E_Binop(ast_id "x", Op_OP_ASGN Op_DIV, ast_id "y", dp)],
  "x=2;x /= y");

]

let suite = "Binary Operator Parse Tests" >:::
  (List.map simple_test tests) @ test_parse_all_builtins ()
