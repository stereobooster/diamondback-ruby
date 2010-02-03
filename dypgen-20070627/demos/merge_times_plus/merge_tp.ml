open Merge_tp_parser
open Merge_tp_lexer
open Parse_tree

let () = Printf.printf "
Example of disambiguation with a merge function
Prints a parse tree

Grammar :
S -> E
E -> int
E -> E + E
E -> E * E

example : 1+2*3 yields
(1+(2*3))

'q' to quit

"
let () = flush stdout

let lexbuf = Lexing.from_channel stdin

let _ = flush stdout

let _ =
  try
    while true do
      (Lexing.flush_input lexbuf;
      try
        let pf = Merge_tp_parser.main Merge_tp_lexer.token lexbuf in
        let _ = List.iter print_tree (List.map (fun (x,_) -> x) pf) in
        print_newline ()
      with
        | Failure s -> Printf.printf "Failure - %s\n\n" s
        | Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Merge_tp_lexer.Eof -> exit 0
