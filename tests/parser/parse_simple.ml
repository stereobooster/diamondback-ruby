
open Ast
open Test_helper
open OUnit

let tests = [
  ("alias", [E_Alias(ast_id "id1", ast_id "id2",dp)], "alias id1 id2");
  ("alias", [E_Alias(ast_id "id1=", ast_id "id2=",dp)],  "alias id1= id2=");
  
  ("BEGIN block", [E_BeginBlock([ast_num 123],dp)], "BEGIN { 123 }");

  ("END block", [E_EndBlock([ast_num 123],dp)], "END { 123 }");

  ("scope operator", [E_Binop(ast_id "X", Op_SCOPE,ast_id "Y",dp)], "X::Y");

  ("uscope operator", 
   [E_Binop(E_Unary(Op_UScope,ast_id "X",dp), Op_SCOPE,ast_id "Y",dp)],
   "::X::Y");

  ("scope operator",
   [E_Binop(E_Binop(ast_id "X",Op_SCOPE,ast_id "Y",dp),
	    Op_SCOPE,ast_id "Z",dp)],
   "X::Y::Z");

  ("hash literal", [E_Hash(true,[],dp)], "{}");
  ("hash literal", [E_Hash(true,[E_Binop(ast_num 1, Op_ASSOC, ast_num 3,dp)],dp)],
   "{1 => 3}");
  ("hash literal", [E_Hash(true,[E_Binop(ast_num 1, Op_ASSOC, ast_num 3,dp)],dp)],
   "{1 => 3,}");
  ("hash literal", [E_Hash(true,[E_Binop(ast_num 1, Op_ASSOC, ast_num 3,dp)],dp)],
   "{1 => 3,\n}");
  ("hash literal",
   [E_Hash(true,[E_Binop(ast_num 1, Op_ASSOC, ast_num 3,dp);
	   E_Binop(ast_num 2, Op_ASSOC, ast_num 4,dp);],dp)],
   "{1 => 3, 2 => 4}");
  ("hash literal", 
   [E_Hash(true,[ast_num 1; ast_num 2; ast_num 3; ast_num 4],dp)],
  "{1,2,3,4}");
  ("hash literal",
   [E_Hash(true,[E_Binop(ast_num 1, Op_ASSOC, ast_num 3,dp);
	   E_Binop(ast_num 2, Op_ASSOC, ast_num 4,dp);],dp)],
   "{1 => 3,\n 2 => 4}");

  ("array literal", [E_Array([],dp)], "[]");
  ("array literal", [E_Array([ast_num 1; ast_num 2],dp)], "[1,2]");
  ("array literal", [E_Array([ast_num 1; ast_num 2],dp)], "[1,\n2]");
  ("array literal", [E_Array([ast_num 1; ast_num 2],dp)], "[1,2,]");
  
  ("multi assign",[E_Binop(E_Tuple([ast_id "x";ast_id "y"],dp),
			   Op_ASSIGN,
			   E_Tuple([ast_num 1; ast_num 2],dp),dp)],
   "x,y = 1,2");

  ("multi assign",
   [E_Binop(
       E_Tuple(
	   [E_Tuple(
	       [E_MethodCall(
		   E_Binop(ast_id "x",Op_DOT,E_Operator(Op_AREF,dp),dp),
		   [ast_num 1;ast_num 2], None,dp);
		E_MethodCall(E_Binop(ast_id "o",Op_DOT,ast_id "y",dp),
			    [],None,dp)
	       ],dp);
	    ast_id "z"],
	   dp),
       Op_ASSIGN,
       E_Tuple([E_Array([ast_num 1; ast_num 2],dp);ast_num 3],dp),
       dp)],
   "(x[1,2],o.y),z = [1,2],3");

  ("return",[E_Return([ast_id "x"],dp)], "return x");
  ("return",[E_Return([ast_id "x"],dp)], "return (x)");

  ("return cond",[E_If (ast_id "y", [E_Return([ast_id "x"],dp)],[],dp)], "return x if y");
  
  ("return multi",
   [E_Return([ast_id "x"; E_Binop(ast_id "y", Op_PLUS, ast_num 2,dp)],dp)],
   "return x,y+2");

  ("return yield",
   [E_Return([E_Yield([],dp)],dp)], 
   "return(yield)");
  ("return yield", 
   [E_Return([E_Yield([],dp)],dp)], 
   "return yield");

  ("return hash",
  [E_Return([E_Hash(true,[],dp)],dp)],
  "return {}");

  ("return uop",
   [E_Return([ast_num (-1); ast_num (-1)],dp)],
   "return -1, -1");
  
  ("super", 
   [E_MethodDef(ast_id "m",[], just_body [ast_id "super"],dp)], 
    "def m();super;end"
   );
   
  ("super()", 
   [E_MethodDef(ast_id "m",[], 
                just_body [E_MethodCall(ast_id "super",[],None,dp)], dp)],
   "def m();super();end"
  );

  ("undef", 
   [E_Undef([ast_id "x"],dp)], 
   "undef x");
];;

let suite = "Simple Expression Parse Tests" >:::
  List.map simple_test tests

  
