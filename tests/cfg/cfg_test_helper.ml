
open OUnit

open Parse_helper
open Cfg
open Cfg_printer

let assert_cfg_equal = 
  assert_equal ~cmp:Cfg.stmt_eq
    ~printer:(fun x -> "\n" ^ (CodePrinter.string_of_cfg x))

let assert_cfg_neq = 
  assert_equal ~cmp:(fun x y -> not (Cfg.stmt_eq x y))
    ~printer:(fun x -> "\n" ^ (CodePrinter.string_of_cfg x))

let refactor_string s = 
  let ast = Parse_helper.parse_string s in
    Cfg_refactor.refactor_ast ast

  
let refactor_test (desc,cfg,code) = 
  let d = desc ^ ": "  ^ code in
    d >:: (fun () -> 
      Cfg_refactor.re_init ();
      let cfg' = refactor_string code in
	assert_cfg_equal cfg cfg'
	  )

let dp = Lexing.dummy_pos
