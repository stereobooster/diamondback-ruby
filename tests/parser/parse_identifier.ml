
open Ast
open Test_helper
open OUnit

let tests = [
  ("lid", [ast_id "id"], "id");
  ("lid", [ast_id "_any"], "_any");
  ("lid", [ast_id "_Any"], "_Any");
  ("instance var", [ast_id "@any"], "@any");
  ("instance var", [ast_id "@Any"], "@Any");
  ("class var", [ast_id "@@any"], "@@any");
  ("class var", [ast_id "@@Any"], "@@Any");
  ("global var", [ast_id "$any"], "$any");
  ("global var", [ast_id "$Any"], "$Any");
  ("builtin var", [E_Identifier(ID_Builtin, "$12",dp)], "$12");
  ("UID",[ast_id "Uid"], "Uid");
]

let suite = "Identifier Parse Tests" >:::
  List.map simple_test tests
