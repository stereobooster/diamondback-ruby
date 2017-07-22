open Sharp_parser
open Sharp_lexer

let () = Printf.printf "
Example of dynamic changes to the grammar

Initial grammar :
S -> E
E -> int
E -> ( E )
E -> A E
A -> & * { action : add rule E -> E # E
              which action is multiplication and
              no shift if the action is executed }
A -> & + { action : add rule E -> E # E
              which action is addition and no
              shift if the action is executed }

Examples of dynamic addition of production rules with the following strings :

&* (3#2# &+ (10#20#7))
# performs multiplication the 2 first times and then addition.
It will be parsed : 3*2*(10+20+7)

&* (3#2# &+(10#20) #7)
will be parsed : 3*2*(10+20)*7

&+ (3#12# &* (5#8# &+ (3#4)))
will be parsed : 3+12+(5*8*(3+4))

'q' to quit

"

let () = flush stdout

let lexbuf = Lexing.from_channel stdin

let _ =
  try
    while true do
      (Lexing.flush_input lexbuf;
      try
        let pf = main Sharp_lexer.token lexbuf in
        Printf.printf "= %d\n\n" (fst (List.hd pf))
      with 
        | Failure s -> Printf.printf "Failure - %s\n\n" s
        | Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Sharp_lexer.Eof -> exit 0
