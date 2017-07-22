open Global_data_parser
open Global_data_lexer

let () = Printf.printf "
Counting the number of reductions with data

Priorities: pi<p*<p+

Grammar:
S -> E
E(pi) -> int
E(p+) -> E(<=p+) + E(<p+)
E(p*) -> E(<=p*) * E(<p*)

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
        let pf = Global_data_parser.main Global_data_lexer.token lexbuf in
        Printf.printf "= %d\n\n" (fst (List.hd pf))
      with
        | Failure s -> Printf.printf "Failure - %s\n\n" s
        | Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Global_data_lexer.Eof -> exit 0
