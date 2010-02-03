
open Ast
open Test_helper
open OUnit

let tests = [

  ("or precedence",
   [E_Binop(E_MethodCall (ast_id "x",
			  [E_Binop(ast_id "y", Op_PLUS, ast_id "z",dp)], 
			  None,dp),
	    Op_kOR,
	   E_Literal(Lit_True,dp),dp)],
   "x y + z or true");

  ("not precedence",
   [E_Unary(Op_UNot, E_Binop(ast_id "x", Op_PLUS, ast_id "y",dp),dp)],
   "not x + y");

  ("! precedence",
   [E_Binop(E_Unary (Op_UBang,ast_id "x",dp), Op_PLUS, ast_id "y",dp)],
   "! x + y");

  ("! precedence",
   [E_Binop(E_Unary (Op_UBang,E_Literal(Lit_True,dp),dp), Op_kOR, E_Literal(Lit_False,dp),dp)],
   "! true or false");

  ("= right assoc",
   [E_Binop(ast_id "a",Op_ASSIGN,E_Binop(ast_id "b",Op_ASSIGN,ast_num 3,dp),dp)],
   "a = b = 3"
  );

  ("- left assoc",
   [E_Binop(
      E_Binop(E_Binop(ast_num 1, Op_MINUS, ast_num 2,dp),
              Op_MINUS,
              ast_num 3, dp),
      Op_MINUS, 
      ast_num 4,
      dp)],
   "1-2-3-4"
  );

  ("unary -",
   [E_Binop(E_Unary (Op_UMinus,ast_id "x",dp), Op_PLUS, ast_id "y",dp)],
   "- x + y");
  
  ("binary -",
   [E_Binop(E_Binop(ast_num 1, Op_PLUS, ast_num 2, dp),
            Op_MINUS,
	    E_Binop(ast_num 3, Op_TIMES, ast_num 4,dp),dp)],

   "1+2-3*4");

  ("binary -",
   [E_Binop(E_Binop(ast_num 1, Op_MINUS, ast_num 2,dp),
	    Op_PLUS, 
	    E_Binop(ast_num 3, Op_TIMES, ast_num 4,dp),dp)],
   "1-2+3*4");

  ("arith/obj call precedence",
   [E_Binop(ast_id "x", Op_PLUS, E_Yield([ast_id "z"],dp),dp)],
   "x + yield ( z )");

  ("binop defined",
   [E_Binop(ast_id "a",Op_OR,
	   E_MethodCall(ast_id "defined?",[ast_id "b"],None,dp),dp)],
   "a || defined?(b)");
  
  ("|| right assign",
   [E_Binop(ast_id "n",Op_ASSIGN,E_Binop(ast_id "x",Op_OR,ast_id "y",dp),dp)],
  "n = x || y"
  );

  ("|| left assign",
   [E_Binop(ast_id "x",Op_OR,E_Binop(ast_id "n",Op_ASSIGN,ast_id "y",dp),dp)],
  "x || n = y"
  );

  ("|| both assign",
   [E_Binop(ast_id "x",Op_OR,E_Binop(ast_id "n",Op_ASSIGN,
				    E_Binop(ast_id "y",Op_OR,ast_id "z",dp)
				      ,dp),dp)],
  "x || n = y || z"
  );

  ("or right assign",
   [E_Binop(E_Binop(ast_id "n",Op_ASSIGN, ast_id "x",dp),Op_kOR,ast_id "y",dp)],
  "n = x or y"
  );

  ("&& ||",
   [E_Binop(E_Binop(ltrue,Op_AND,lfalse,dp),Op_OR,ltrue,dp)],
   "true && false || true"
  );

  ("assign range",
   [E_Binop(ast_id "x",Op_ASSIGN,E_Binop(ast_num 1,Op_DOT2,ast_num 2,dp),dp)],
   "x = 1..2"
  );

  ("+ range",
   [E_Binop(E_Binop(ast_num 1, Op_PLUS, ast_num 2,dp),Op_DOT2,ast_num 3,dp)],
   "1+2..3"
  );

  ("tern arg",
   [E_Ternary(ast_id "x", ast_num 0, E_Binop(ast_id "y", Op_PLUS,ast_num 2, dp),dp)],
   "x ? 0 : y+2"
  );

  ("multi ||",
   [E_Binop(ast_id "z",Op_ASSIGN,
            E_Binop(E_Binop(ltrue,Op_OR,lfalse,dp),Op_OR,ltrue,dp),dp)],
   "z = true || false || true"
  );
]

let suite = "Precedence Parse Tests" >:::
  List.map simple_test tests
