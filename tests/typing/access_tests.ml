
open Ast
open Parse_helper
open OUnit
open Config

module T = Typing

let build_test (d,l) =  d >:: fun () -> 
  conf.error_raises_exc <- true;
  let ast = Parse_helper.parse_string l in
  let cfg = Cfg_refactor.refactor_ast ast in
  let loader = File_loader.create File_loader.EmptyCfg [] in
    ignore(T.type_cfg loader "<test>" cfg)

let tests = [
  ("public","
class A
  def f() end
end
A.new.f
");
]
  
let suite = "Method Access" >:::
  List.map build_test tests

