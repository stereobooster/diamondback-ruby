
open Ast
open Parse_helper
open OUnit
open Config

module TypeProgs = struct
  let dir = "typing"
  let success_dir = "typing/succeed"
  let fail_dir = "typing/fail"
  let setup _ = ()
  let teardown _ = ()
end

module AnnotProgs = struct
  let dir = "typing"
  let success_dir = "typing/annot_succeed"
  let fail_dir = "typing/annot_fail"
  let setup _ = ()
  let teardown _ = ()
end

module TypeTests = Prog_test.Make(TypeProgs)
module AnnotTests = Prog_test.Make(AnnotProgs)

let suite = "Type Progs" >:::
  [TypeTests.suite;
   AnnotTests.suite;
  ]
  (*List.fold_left List.rev_append [] tests_list*)
