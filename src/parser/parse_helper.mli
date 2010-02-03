
val parse_ruby_lexbuf : Lexing.lexbuf -> Ast.ast

val parse_interface_lexbuf : Lexing.lexbuf -> Annotation.interface

val parse_ruby_string : ?env:Utils.StrSet.t -> ?filename:string -> ?lineno:int
  -> string -> Ast.ast

val parse_interface_string : string -> Annotation.interface

val parse_ruby_file : string -> Ast.ast

val parse_interface_file : string -> Annotation.interface
