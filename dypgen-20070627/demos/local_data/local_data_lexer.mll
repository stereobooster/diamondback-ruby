{
open Local_data_parser
exception Eof
}
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
    [' ' '\t']        { token lexbuf }     (* skip blanks *)
  | ['\n' ]           { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'               { PLUS }
  | "in"               { IN }
  | '='               { EQUAL }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | "let"               { LET }
  | lowercase identchar *
      { IDENT(Lexing.lexeme lexbuf) }
  | eof | 'Q' | 'E'   { raise Eof }
