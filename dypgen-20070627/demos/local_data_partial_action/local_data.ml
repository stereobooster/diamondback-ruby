open Local_data_parser
open Local_data_lexer
open Parse_tree

let () = Printf.printf "
Example of use of local_data to build a simple symbol table
and to disambiguate.

Grammar :
S -> E
E -> int
E -> id
E -> ( E )
E -> E + E
E -> let B in E
B -> id = E

example :

`let x = 1 in x+1'
yields :
  (let x = 1 in (x+1))
  ((let x = 1 in x)+1)

`let x = 1 in 1+x'
only yields :
  (let x = 1 in (1+x))

'Q' to quit

"
let () = flush stdout

let lexbuf = Lexing.from_channel stdin

let _ = flush stdout

let _ =
  try
    while true do
      (Lexing.flush_input lexbuf;
      try
        let pf = Local_data_parser.main Local_data_lexer.token lexbuf in
        let _ = List.iter print_tree (List.map (fun (x,_) -> x) pf) in
        print_newline ()
      with
        | Failure s -> Printf.printf "Failure - %s\n\n" s
        | Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Local_data_lexer.Eof -> exit 0
