{
open Sharp_parser
exception Eof
}
rule token = parse
    [' ' '\t']        { token lexbuf }     (* skip blanks *)
  | ['\n' ]           { Eof_token }
  | ['0'-'9']+ as lxm { Int(int_of_string lxm) }
  | '+'               { Plus }
  | '*'               { Time }
  | '#'               { Sharp }
  | '&'               { And_token }
  | '('               { Lpar }
  | ')'               { Rpar }
  | eof | 'q' | 'e'   { raise Eof }
