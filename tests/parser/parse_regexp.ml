
open Ast
open Test_helper
open OUnit

let tests = [
  ("regular expression", [ast_re "1234" ""], "/1234/");
  ("regexp interpolate", 
   [E_Literal(Lit_Regexp ([StrChars "123";StrExpr (ast_num 4)],""),dp)],
   "/123#{4}/"
  );
  ("token after interp",
   [E_Literal(Lit_Regexp([StrChars "(?:3";StrExpr (ast_id "integer");
                          StrChars ")"],"io"),dp)],
   "/(?:3#{integer})/io"
  );

  ("% regexp modifier", [ast_re "a" "i"], "%r{a}i");
  ("% regexp escape {", [ast_re "{" ""], "%r{\\{}");

  ("escaped newline",
   [ast_re "3" ""],
   "/\\\n3/"
  );
]

let suite = "Regexp Parse Tests" >:::
  List.map simple_test tests
