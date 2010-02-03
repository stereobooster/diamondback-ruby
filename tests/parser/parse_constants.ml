
open Ast
open Test_helper
open OUnit

let tests = [
  ("bignum",
   [E_Literal(Lit_BigNum (Big_int.big_int_of_string "36893488147419103232"),dp)],
   "36893488147419103232");

  ("hex bignum",
   [E_Literal(Lit_BigNum (Big_int.big_int_of_string "4611686018427387905"),dp)],
   "0x4000_0000_0000_0001");

  ("octal 0", [E_Literal(Lit_FixNum 42807,dp)], "0123467");
  ("octal o", [E_Literal(Lit_FixNum 42807,dp)], "0o123467");
  ("octal O", [E_Literal(Lit_FixNum 42807,dp)], "0O123467");

  ("octal _", [E_Literal(Lit_FixNum 42807,dp)], "0O1_234_6_7");

  ("octal bignum",
   [E_Literal(Lit_BigNum (Big_int.big_int_of_string "9223372036854775807"),dp)],
   "0777777777777777777777");

  ("fixnum",[ast_num 1234], "1234");
  ("binary fixnum", [ast_num 15], "0b1111");
  ("float", [ast_float 3.1], "3.1");
  ("exp", [ast_float 3.1e10], "3.1e10");
  ("+exp", [ast_float 3.7e17], "3.7e+17");

  ("atom",  [ast_atom "x"],  ":x");
  ("atom operator", [ast_atom ">="],  ":>=");
  ("interp in atom string",
   [E_Literal(Lit_Atom [StrChars "a";StrExpr (ast_id "b")], dp)], 
  ":\"a#{b}\"");

  ("nil",   [E_Literal(Lit_Nil,dp)],       "nil");
  ("yield", [E_Yield([],dp)],      "yield");

  ("true",  [E_Literal(Lit_True,dp)],  "true");
  ("false", [E_Literal(Lit_False,dp)], "false");
  ("break", [E_Identifier(ID_Lowercase, "break",dp)], "break");
  ("redo",  [E_Identifier(ID_Lowercase, "redo",dp)],  "redo");
  ("next",  [E_Identifier(ID_Lowercase, "next",dp)],  "next");
  ("atom string", [ast_atom "hi there"], ":\"hi there\"");
  ("atom single string", [ast_atom "hi #{3} there"], ":'hi #{3} there'");
  
  ("?a", [ast_num 97], "?a");
  ("?\\C-a", [ast_num 1], "?\\C-a");
  ("?\\M-a", [ast_num 225], "?\\M-a");
  ("?\\C-\\M-a", [ast_num 129], "?\\C-\\M-a");
  ("?\\M-\\C-a", [ast_num 129], "?\\M-\\C-a");
]

let ignores = [
]

let suite = "Constant Parse Tests" >:::
  List.map simple_test tests @ List.map simple_test_ignore ignores
