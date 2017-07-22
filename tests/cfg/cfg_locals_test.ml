
open OUnit
open Cfg_test_helper
open Cfg
open Utils  

module C = Cfg.Abbr
  


let local_test valid (desc,pre,local) = 
  desc >:: 
    (fun () ->
       let post = empty_stmt () in
       let cfg = C.seq [pre;post] dp in
       compute_cfg_locals cfg;
       let msg = Printf.sprintf "local %spresent: %s" 
         (if valid then "not " else "") local 
       in
       let mem = StrSet.mem local post.lexical_locals in
       let x_or = if valid then mem else (not mem) in
         assert_bool msg x_or
    )

let true_tests = [
  ("assignment",
   C.assign (C.local "x") (C.num 3) dp,
   "x"
  );

  ("mcall",
   refactor_string "x = 2+3",
   "x"
  );

  ("yield",
   refactor_string "x = yield(2)",
   "x"
  );

]

let false_tests = [
  ("assignment fail",
   C.assign (C.local "x") (C.num 3) dp,
   "y"
  );

  ("mcall",
   refactor_string "x = 2+3",
   "y"
  );

  ("yield",
   refactor_string "x = yield(2)",
   "y"
  );

  ("mdef",
   refactor_string "def f() x = 2+3 end",
   "x"
  );

  ("class",
   refactor_string "class A; x = 2+3 end",
   "x"
  );

  ("module",
   refactor_string "module M; x = 2+3 end",
   "x"
  );

  ("begin",
   refactor_string "BEGIN {x = 3}",
   "x"
  );

  ("end",
   refactor_string "END {x = 3}",
   "x"
  );
(*
  | Seq of stmt list
  | Alias of alias_kind
  | If of expr * stmt * stmt
  | Case of case_block
  | While of expr * stmt
  | For of block_formal_param list * expr * stmt 
  | MethodCall of lhs option * method_call
  | Assign of lhs * tuple_expr
  | Expression of expr
  | Return of tuple_expr option
  | Yield of lhs option * method_arg_expr list
  | Module of lhs option * identifier * stmt
  | Method of def_name * method_formal_param list * stmt
  | Class of lhs option * class_kind * stmt
  | ExnBlock of exn_block
  | Begin of stmt 
  | End of stmt 
  | Defined of identifier * stmt (* id = defined(stmt) *)
  | Undef of msg_id list
  | Break of tuple_expr option
  | Next of tuple_expr option
  | Redo
  | Retry
  | Cast of identifier
*)
]

let suite = "Locals suite" >:::
  (List.map (local_test true) true_tests) @ 
  (List.map (local_test false) false_tests)
