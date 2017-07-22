type priority
type token = | EOF | LIDENT of (string * (int * int * int)) | UIDENT of (string * (int * int * int)) | PATTERN of (string * (int * int)) | OCAML_TYPE of (string) | OCAML_CODE of (string * (int * int)) | KWD_NON_TERMINAL | KWD_TYPE | KWD_FOR | KWD_CONSTRUCTOR | KWD_MERGE | KWD_MLI | KWD_RELATION | KWD_START | KWD_TOKEN | EQUAL | GREATER | LESS | BAR | RBRACE | LBRACE | PERCENTPERCENT | COLON | SEMI | COMMA | RPAREN | LPAREN
val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_tree.obj * priority) list
