
open Ast
open Test_helper
open OUnit

let lpos l = 
  {Lexing.pos_fname = "";
   pos_lnum = l;
   pos_bol = 0;
   pos_cnum = 0;
  }

let cmp_line p1 p2 = p1.Lexing.pos_lnum = p2.Lexing.pos_lnum

let rec last = function
  | [] -> raise (Invalid_argument "last")
  | [x] -> x
  | hd::tl -> last tl

let cmp_pos ast1 ast2 = 
  cmp_line (Ast.pos_of (last ast1)) (Ast.pos_of (last ast2))

let printer ast = 
  let p = Ast.pos_of (last ast) in
    Printf.sprintf "%s(%d)" (Ast_printer.string_of_ast ast) 
      (p.Lexing.pos_lnum)

let pos_test (desc,code,line) = 
  desc >:: (fun () -> 
	      let exp = Parse_helper.parse_string (code ^ "\nx") in
	      let got0 = Parse_helper.parse_string code in
              let got = got0 @ [E_Identifier(ID_Lowercase,"x",lpos line)] in
	        assert_equal exp got ~cmp:cmp_pos ~printer:printer
	   )

let pos_local id line = 
  E_Identifier(ID_Lowercase, id, dp)


let tests = [
  ("position after empty",
   "",
   2);

  ("position after id",
   "y",
   2);

  ("position after newline",
   "y\n",
   3);

  ("position after heredoc",
   "<<EOF
2
3
EOF",
   5);

  ("position after 2 heredoc",
   "<<EOF
2
3
EOF
<<EOF
6
7
EOF",
   9);

]

let suite = "Position Tests" >:::
  List.map pos_test tests
