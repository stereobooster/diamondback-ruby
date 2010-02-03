
open OUnit

open Parse_helper
open Cfg
open Cfg_printer

let string_of_load_result l_result = match l_result with
  | File_loader.Ruby(cfg) ->  (CodePrinter.string_of_cfg cfg) 
  | File_loader.Interface(iface) -> (CodePrinter.string_of_cfg iface) 
  | File_loader.Native(s) -> "Native: " ^ s

let assert_load_result_equal =
  assert_equal ~cmp:File_loader.eql_load_result
    ~printer:string_of_load_result

let assert_load_result_neq =
  assert_equal ~cmp:(fun x y -> not (File_loader.eql_load_result x y))
    ~printer:string_of_load_result

let assert_cfg_equal = 
  assert_equal ~cmp:Cfg.stmt_eq
    ~printer:(fun x -> "\n" ^ (CodePrinter.string_of_cfg x))

let assert_cfg_neq = 
  assert_equal ~cmp:(fun x y -> not (Cfg.stmt_eq x y))
    ~printer:(fun x -> "\n" ^ (CodePrinter.string_of_cfg x))

let refactor_string_cfg s =
  let ast = Parse_helper.parse_ruby_string s in
    Cfg_refactor.refactor_ast ast 

let refactor_string s = 
    File_loader.Ruby(refactor_string_cfg s)
  
let refactor_test (desc,cfg,code) = 
  let d = desc ^ ": "  ^ code in
    d >:: (fun () -> 
      Cfg_refactor.re_init ();
      let cfg' = refactor_string code in
	assert_load_result_equal (File_loader.Ruby(cfg)) cfg'
	  )

let dp = Lexing.dummy_pos
