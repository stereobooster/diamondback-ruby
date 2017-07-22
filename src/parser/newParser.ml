type token =
  | T_EOF
  | T_EOL
  | T_INTERP_END of (string * Lexing.position)
  | T_INTERP_STR of (string * Lexing.position)
  | T_ATOM_BEG of (Lexing.position)
  | T_REGEXP of (Ast.interp_string * string * Lexing.position)
  | T_REGEXP_MOD of (string)
  | T_REGEXP_BEG of (Lexing.position)
  | T_USER_BEG of (string * Lexing.position)
  | T_TICK_BEG of (Lexing.position)
  | T_DOUBLE_BEG of (Lexing.position)
  | T_USER_STRING of (string * Ast.interp_string * Lexing.position)
  | T_DOUBLE_STRING of (Ast.interp_string * Lexing.position)
  | T_SINGLE_STRING of (string * Lexing.position)
  | K_FALSE of (Lexing.position)
  | K_TRUE of (Lexing.position)
  | K_SELF of (Lexing.position)
  | K_YIELD of (Lexing.position)
  | K_NIL of (Lexing.position)
  | K_lEND of (Lexing.position)
  | K_lBEGIN of (Lexing.position)
  | K_NOT of (Lexing.position)
  | K_OR of (Lexing.position)
  | K_AND of (Lexing.position)
  | K_RETURN of (Lexing.position)
  | K_DO of (Lexing.position)
  | K_IN of (Lexing.position)
  | K_FOR of (Lexing.position)
  | K_UNTIL of (Lexing.position)
  | K_WHILE of (Lexing.position)
  | K_WHEN of (Lexing.position)
  | K_CASE of (Lexing.position)
  | K_ELSE of (Lexing.position)
  | K_ELSIF of (Lexing.position)
  | K_THEN of (Lexing.position)
  | K_UNLESS of (Lexing.position)
  | K_IF of (Lexing.position)
  | K_ENSURE of (Lexing.position)
  | K_RESCUE of (Lexing.position)
  | K_BEGIN of (Lexing.position)
  | K_UNDEF of (Lexing.position)
  | K_ALIAS of (Lexing.position)
  | K_END of (Lexing.position)
  | K_DEF of (string*Lexing.position)
  | K_MODULE of (string*Lexing.position)
  | K_CLASS of (string*Lexing.position)
  | T_BUILTIN_VAR of (string * Lexing.position)
  | T_FLOAT of (string * float * Lexing.position)
  | T_BIGNUM of (Big_int.big_int * Lexing.position)
  | T_FIXNUM of (int * Lexing.position)
  | T_ATOM of (Ast.interp_string * Lexing.position)
  | T_CLASS_VAR of (string * Lexing.position)
  | T_INST_VAR of (string * Lexing.position)
  | T_GLOBAL_VAR of (string * Lexing.position)
  | T_LID of (string * Lexing.position)
  | T_UID of (string * Lexing.position)
  | T_CAST of (string * string * Lexing.position)
  | T_USCOPE of (Lexing.position)
  | T_SCOPE of (Lexing.position)
  | T_SEMICOLON of (Lexing.position)
  | T_UAMPER of (Lexing.position)
  | T_AMPER of (Lexing.position)
  | T_TILDE of (Lexing.position)
  | T_BANG of (Lexing.position)
  | T_VBAR of (Lexing.position)
  | T_CARROT of (Lexing.position)
  | T_COLON of (Lexing.position)
  | T_QUESTION of (Lexing.position)
  | T_PERCENT of (Lexing.position)
  | T_SLASH of (Lexing.position)
  | T_USTAR of (Lexing.position)
  | T_STAR of (Lexing.position)
  | T_RBRACE of (Lexing.position)
  | T_LBRACE_ARG of (Lexing.position)
  | T_LBRACE of (Lexing.position)
  | T_RBRACK of (Lexing.position)
  | T_LBRACK of (Lexing.position)
  | T_LBRACK_ARG of (Lexing.position)
  | T_RPAREN of (Lexing.position)
  | T_LPAREN_ARG of (Lexing.position)
  | T_LPAREN of (Lexing.position)
  | T_ASSOC of (Lexing.position)
  | T_OP_ASGN of (string*Lexing.position)
  | T_RSHFT of (Lexing.position)
  | T_LSHFT of (Lexing.position)
  | T_DOT3 of (Lexing.position)
  | T_DOT2 of (Lexing.position)
  | T_NMATCH of (Lexing.position)
  | T_MATCH of (Lexing.position)
  | T_OROP of (Lexing.position)
  | T_ANDOP of (Lexing.position)
  | T_GT of (Lexing.position)
  | T_LT of (Lexing.position)
  | T_LEQ of (Lexing.position)
  | T_GEQ of (Lexing.position)
  | T_NEQ of (Lexing.position)
  | T_EQQ of (Lexing.position)
  | T_EQ of (Lexing.position)
  | T_ASSIGN of (Lexing.position)
  | T_CMP of (Lexing.position)
  | T_POW of (Lexing.position)
  | T_UMINUS of (Lexing.position)
  | T_MINUS of (Lexing.position)
  | T_UPLUS of (Lexing.position)
  | T_PLUS of (Lexing.position)
  | T_COMMA of (Lexing.position)
  | T_DOT of (Lexing.position)

module Dyp_symbols =
struct
  let arg = 1
  let arg_comma_list_trail = 2
  let arg_comma_nonempty_list = 3
  let arg_comma_star_list = 4
  let array_body = 5
  let array_body_rest = 6
  let array_item = 7
  let assign_op = 8
  let assignable = 9
  let bin_op = 10
  let body_exn = 11
  let brace_codeblock = 12
  let call_args = 13
  let case_else = 14
  let class_inheritance = 15
  let code_block = 16
  let code_block_body = 17
  let command = 18
  let command_codeblock = 19
  let command_name = 20
  let constant = 21
  let do_codeblock = 22
  let do_sep = 23
  let ensure_clause = 24
  let eol_or_semi = 25
  let eols = 26
  let expr = 27
  let formal_arg = 28
  let formal_arg_list = 29
  let formal_arg_nonempty_list = 30
  let func = 31
  let identifier = 32
  let if_else_clauses = 33
  let interp_code = 34
  let interp_str = 35
  let interp_str_work = 36
  let keyword_as_id = 37
  let lhs = 38
  let lhs_assign_op = 39
  let lhs_prune_binop = 40
  let lhs_pruned_assign_op = 41
  let lparen = 42
  let main = 43
  let message_identifier = 44
  let meth_or_atom = 45
  let meth_or_atom_list = 46
  let method_formals = 47
  let method_name = 48
  let mlhs = 49
  let mlhs_assign_op = 50
  let mlhs_clean = 51
  let mlhs_item = 52
  let mlhs_rest = 53
  let mrhs = 54
  let one_string = 55
  let primary = 56
  let rescue_clause = 57
  let rescue_list = 58
  let rescue_list_rest = 59
  let scope_begin = 60
  let scope_class = 61
  let scope_def = 62
  let scope_end = 63
  let scope_module = 64
  let scoped_id = 65
  let seen_id = 66
  let some_eols = 67
  let star_amper = 68
  let stmt = 69
  let stmt_list = 70
  let string = 71
  let then_sep = 72
  let topcall = 73
  let unary_op = 74
  let when_clauses = 75
  let t_T_EOF = 2
  let t_T_EOL = 3
  let t_T_INTERP_END = 4
  let t_T_INTERP_STR = 5
  let t_T_ATOM_BEG = 6
  let t_T_REGEXP = 7
  let t_T_REGEXP_MOD = 8
  let t_T_REGEXP_BEG = 9
  let t_T_USER_BEG = 10
  let t_T_TICK_BEG = 11
  let t_T_DOUBLE_BEG = 12
  let t_T_USER_STRING = 13
  let t_T_DOUBLE_STRING = 14
  let t_T_SINGLE_STRING = 15
  let t_K_FALSE = 16
  let t_K_TRUE = 17
  let t_K_SELF = 18
  let t_K_YIELD = 19
  let t_K_NIL = 20
  let t_K_lEND = 21
  let t_K_lBEGIN = 22
  let t_K_NOT = 23
  let t_K_OR = 24
  let t_K_AND = 25
  let t_K_RETURN = 26
  let t_K_DO = 27
  let t_K_IN = 28
  let t_K_FOR = 29
  let t_K_UNTIL = 30
  let t_K_WHILE = 31
  let t_K_WHEN = 32
  let t_K_CASE = 33
  let t_K_ELSE = 34
  let t_K_ELSIF = 35
  let t_K_THEN = 36
  let t_K_UNLESS = 37
  let t_K_IF = 38
  let t_K_ENSURE = 39
  let t_K_RESCUE = 40
  let t_K_BEGIN = 41
  let t_K_UNDEF = 42
  let t_K_ALIAS = 43
  let t_K_END = 44
  let t_K_DEF = 45
  let t_K_MODULE = 46
  let t_K_CLASS = 47
  let t_T_BUILTIN_VAR = 48
  let t_T_FLOAT = 49
  let t_T_BIGNUM = 50
  let t_T_FIXNUM = 51
  let t_T_ATOM = 52
  let t_T_CLASS_VAR = 53
  let t_T_INST_VAR = 54
  let t_T_GLOBAL_VAR = 55
  let t_T_LID = 56
  let t_T_UID = 57
  let t_T_CAST = 58
  let t_T_USCOPE = 59
  let t_T_SCOPE = 60
  let t_T_SEMICOLON = 61
  let t_T_UAMPER = 62
  let t_T_AMPER = 63
  let t_T_TILDE = 64
  let t_T_BANG = 65
  let t_T_VBAR = 66
  let t_T_CARROT = 67
  let t_T_COLON = 68
  let t_T_QUESTION = 69
  let t_T_PERCENT = 70
  let t_T_SLASH = 71
  let t_T_USTAR = 72
  let t_T_STAR = 73
  let t_T_RBRACE = 74
  let t_T_LBRACE_ARG = 75
  let t_T_LBRACE = 76
  let t_T_RBRACK = 77
  let t_T_LBRACK = 78
  let t_T_LBRACK_ARG = 79
  let t_T_RPAREN = 80
  let t_T_LPAREN_ARG = 81
  let t_T_LPAREN = 82
  let t_T_ASSOC = 83
  let t_T_OP_ASGN = 84
  let t_T_RSHFT = 85
  let t_T_LSHFT = 86
  let t_T_DOT3 = 87
  let t_T_DOT2 = 88
  let t_T_NMATCH = 89
  let t_T_MATCH = 90
  let t_T_OROP = 91
  let t_T_ANDOP = 92
  let t_T_GT = 93
  let t_T_LT = 94
  let t_T_LEQ = 95
  let t_T_GEQ = 96
  let t_T_NEQ = 97
  let t_T_EQQ = 98
  let t_T_EQ = 99
  let t_T_ASSIGN = 100
  let t_T_CMP = 101
  let t_T_POW = 102
  let t_T_UMINUS = 103
  let t_T_MINUS = 104
  let t_T_UPLUS = 105
  let t_T_PLUS = 106
  let t_T_COMMA = 107
  let t_T_DOT = 108
  let get_token_name t = match t with
    | T_EOF -> t_T_EOF
    | T_EOL -> t_T_EOL
    | T_INTERP_END _ -> t_T_INTERP_END
    | T_INTERP_STR _ -> t_T_INTERP_STR
    | T_ATOM_BEG _ -> t_T_ATOM_BEG
    | T_REGEXP _ -> t_T_REGEXP
    | T_REGEXP_MOD _ -> t_T_REGEXP_MOD
    | T_REGEXP_BEG _ -> t_T_REGEXP_BEG
    | T_USER_BEG _ -> t_T_USER_BEG
    | T_TICK_BEG _ -> t_T_TICK_BEG
    | T_DOUBLE_BEG _ -> t_T_DOUBLE_BEG
    | T_USER_STRING _ -> t_T_USER_STRING
    | T_DOUBLE_STRING _ -> t_T_DOUBLE_STRING
    | T_SINGLE_STRING _ -> t_T_SINGLE_STRING
    | K_FALSE _ -> t_K_FALSE
    | K_TRUE _ -> t_K_TRUE
    | K_SELF _ -> t_K_SELF
    | K_YIELD _ -> t_K_YIELD
    | K_NIL _ -> t_K_NIL
    | K_lEND _ -> t_K_lEND
    | K_lBEGIN _ -> t_K_lBEGIN
    | K_NOT _ -> t_K_NOT
    | K_OR _ -> t_K_OR
    | K_AND _ -> t_K_AND
    | K_RETURN _ -> t_K_RETURN
    | K_DO _ -> t_K_DO
    | K_IN _ -> t_K_IN
    | K_FOR _ -> t_K_FOR
    | K_UNTIL _ -> t_K_UNTIL
    | K_WHILE _ -> t_K_WHILE
    | K_WHEN _ -> t_K_WHEN
    | K_CASE _ -> t_K_CASE
    | K_ELSE _ -> t_K_ELSE
    | K_ELSIF _ -> t_K_ELSIF
    | K_THEN _ -> t_K_THEN
    | K_UNLESS _ -> t_K_UNLESS
    | K_IF _ -> t_K_IF
    | K_ENSURE _ -> t_K_ENSURE
    | K_RESCUE _ -> t_K_RESCUE
    | K_BEGIN _ -> t_K_BEGIN
    | K_UNDEF _ -> t_K_UNDEF
    | K_ALIAS _ -> t_K_ALIAS
    | K_END _ -> t_K_END
    | K_DEF _ -> t_K_DEF
    | K_MODULE _ -> t_K_MODULE
    | K_CLASS _ -> t_K_CLASS
    | T_BUILTIN_VAR _ -> t_T_BUILTIN_VAR
    | T_FLOAT _ -> t_T_FLOAT
    | T_BIGNUM _ -> t_T_BIGNUM
    | T_FIXNUM _ -> t_T_FIXNUM
    | T_ATOM _ -> t_T_ATOM
    | T_CLASS_VAR _ -> t_T_CLASS_VAR
    | T_INST_VAR _ -> t_T_INST_VAR
    | T_GLOBAL_VAR _ -> t_T_GLOBAL_VAR
    | T_LID _ -> t_T_LID
    | T_UID _ -> t_T_UID
    | T_CAST _ -> t_T_CAST
    | T_USCOPE _ -> t_T_USCOPE
    | T_SCOPE _ -> t_T_SCOPE
    | T_SEMICOLON _ -> t_T_SEMICOLON
    | T_UAMPER _ -> t_T_UAMPER
    | T_AMPER _ -> t_T_AMPER
    | T_TILDE _ -> t_T_TILDE
    | T_BANG _ -> t_T_BANG
    | T_VBAR _ -> t_T_VBAR
    | T_CARROT _ -> t_T_CARROT
    | T_COLON _ -> t_T_COLON
    | T_QUESTION _ -> t_T_QUESTION
    | T_PERCENT _ -> t_T_PERCENT
    | T_SLASH _ -> t_T_SLASH
    | T_USTAR _ -> t_T_USTAR
    | T_STAR _ -> t_T_STAR
    | T_RBRACE _ -> t_T_RBRACE
    | T_LBRACE_ARG _ -> t_T_LBRACE_ARG
    | T_LBRACE _ -> t_T_LBRACE
    | T_RBRACK _ -> t_T_RBRACK
    | T_LBRACK _ -> t_T_LBRACK
    | T_LBRACK_ARG _ -> t_T_LBRACK_ARG
    | T_RPAREN _ -> t_T_RPAREN
    | T_LPAREN_ARG _ -> t_T_LPAREN_ARG
    | T_LPAREN _ -> t_T_LPAREN
    | T_ASSOC _ -> t_T_ASSOC
    | T_OP_ASGN _ -> t_T_OP_ASGN
    | T_RSHFT _ -> t_T_RSHFT
    | T_LSHFT _ -> t_T_LSHFT
    | T_DOT3 _ -> t_T_DOT3
    | T_DOT2 _ -> t_T_DOT2
    | T_NMATCH _ -> t_T_NMATCH
    | T_MATCH _ -> t_T_MATCH
    | T_OROP _ -> t_T_OROP
    | T_ANDOP _ -> t_T_ANDOP
    | T_GT _ -> t_T_GT
    | T_LT _ -> t_T_LT
    | T_LEQ _ -> t_T_LEQ
    | T_GEQ _ -> t_T_GEQ
    | T_NEQ _ -> t_T_NEQ
    | T_EQQ _ -> t_T_EQQ
    | T_EQ _ -> t_T_EQ
    | T_ASSIGN _ -> t_T_ASSIGN
    | T_CMP _ -> t_T_CMP
    | T_POW _ -> t_T_POW
    | T_UMINUS _ -> t_T_UMINUS
    | T_MINUS _ -> t_T_MINUS
    | T_UPLUS _ -> t_T_UPLUS
    | T_PLUS _ -> t_T_PLUS
    | T_COMMA _ -> t_T_COMMA
    | T_DOT _ -> t_T_DOT
  let str_token t = match t with
    | T_EOF -> "T_EOF"
    | T_EOL -> "T_EOL"
    | T_INTERP_END _ -> "T_INTERP_END"
    | T_INTERP_STR _ -> "T_INTERP_STR"
    | T_ATOM_BEG _ -> "T_ATOM_BEG"
    | T_REGEXP _ -> "T_REGEXP"
    | T_REGEXP_MOD s -> "T_REGEXP_MOD("^s^")"
    | T_REGEXP_BEG _ -> "T_REGEXP_BEG"
    | T_USER_BEG _ -> "T_USER_BEG"
    | T_TICK_BEG _ -> "T_TICK_BEG"
    | T_DOUBLE_BEG _ -> "T_DOUBLE_BEG"
    | T_USER_STRING _ -> "T_USER_STRING"
    | T_DOUBLE_STRING _ -> "T_DOUBLE_STRING"
    | T_SINGLE_STRING _ -> "T_SINGLE_STRING"
    | K_FALSE _ -> "K_FALSE"
    | K_TRUE _ -> "K_TRUE"
    | K_SELF _ -> "K_SELF"
    | K_YIELD _ -> "K_YIELD"
    | K_NIL _ -> "K_NIL"
    | K_lEND _ -> "K_lEND"
    | K_lBEGIN _ -> "K_lBEGIN"
    | K_NOT _ -> "K_NOT"
    | K_OR _ -> "K_OR"
    | K_AND _ -> "K_AND"
    | K_RETURN _ -> "K_RETURN"
    | K_DO _ -> "K_DO"
    | K_IN _ -> "K_IN"
    | K_FOR _ -> "K_FOR"
    | K_UNTIL _ -> "K_UNTIL"
    | K_WHILE _ -> "K_WHILE"
    | K_WHEN _ -> "K_WHEN"
    | K_CASE _ -> "K_CASE"
    | K_ELSE _ -> "K_ELSE"
    | K_ELSIF _ -> "K_ELSIF"
    | K_THEN _ -> "K_THEN"
    | K_UNLESS _ -> "K_UNLESS"
    | K_IF _ -> "K_IF"
    | K_ENSURE _ -> "K_ENSURE"
    | K_RESCUE _ -> "K_RESCUE"
    | K_BEGIN _ -> "K_BEGIN"
    | K_UNDEF _ -> "K_UNDEF"
    | K_ALIAS _ -> "K_ALIAS"
    | K_END _ -> "K_END"
    | K_DEF _ -> "K_DEF"
    | K_MODULE _ -> "K_MODULE"
    | K_CLASS _ -> "K_CLASS"
    | T_BUILTIN_VAR _ -> "T_BUILTIN_VAR"
    | T_FLOAT _ -> "T_FLOAT"
    | T_BIGNUM _ -> "T_BIGNUM"
    | T_FIXNUM _ -> "T_FIXNUM"
    | T_ATOM _ -> "T_ATOM"
    | T_CLASS_VAR _ -> "T_CLASS_VAR"
    | T_INST_VAR _ -> "T_INST_VAR"
    | T_GLOBAL_VAR _ -> "T_GLOBAL_VAR"
    | T_LID _ -> "T_LID"
    | T_UID _ -> "T_UID"
    | T_CAST _ -> "T_CAST"
    | T_USCOPE _ -> "T_USCOPE"
    | T_SCOPE _ -> "T_SCOPE"
    | T_SEMICOLON _ -> "T_SEMICOLON"
    | T_UAMPER _ -> "T_UAMPER"
    | T_AMPER _ -> "T_AMPER"
    | T_TILDE _ -> "T_TILDE"
    | T_BANG _ -> "T_BANG"
    | T_VBAR _ -> "T_VBAR"
    | T_CARROT _ -> "T_CARROT"
    | T_COLON _ -> "T_COLON"
    | T_QUESTION _ -> "T_QUESTION"
    | T_PERCENT _ -> "T_PERCENT"
    | T_SLASH _ -> "T_SLASH"
    | T_USTAR _ -> "T_USTAR"
    | T_STAR _ -> "T_STAR"
    | T_RBRACE _ -> "T_RBRACE"
    | T_LBRACE_ARG _ -> "T_LBRACE_ARG"
    | T_LBRACE _ -> "T_LBRACE"
    | T_RBRACK _ -> "T_RBRACK"
    | T_LBRACK _ -> "T_LBRACK"
    | T_LBRACK_ARG _ -> "T_LBRACK_ARG"
    | T_RPAREN _ -> "T_RPAREN"
    | T_LPAREN_ARG _ -> "T_LPAREN_ARG"
    | T_LPAREN _ -> "T_LPAREN"
    | T_ASSOC _ -> "T_ASSOC"
    | T_OP_ASGN _ -> "T_OP_ASGN"
    | T_RSHFT _ -> "T_RSHFT"
    | T_LSHFT _ -> "T_LSHFT"
    | T_DOT3 _ -> "T_DOT3"
    | T_DOT2 _ -> "T_DOT2"
    | T_NMATCH _ -> "T_NMATCH"
    | T_MATCH _ -> "T_MATCH"
    | T_OROP _ -> "T_OROP"
    | T_ANDOP _ -> "T_ANDOP"
    | T_GT _ -> "T_GT"
    | T_LT _ -> "T_LT"
    | T_LEQ _ -> "T_LEQ"
    | T_GEQ _ -> "T_GEQ"
    | T_NEQ _ -> "T_NEQ"
    | T_EQQ _ -> "T_EQQ"
    | T_EQ _ -> "T_EQ"
    | T_ASSIGN _ -> "T_ASSIGN"
    | T_CMP _ -> "T_CMP"
    | T_POW _ -> "T_POW"
    | T_UMINUS _ -> "T_UMINUS"
    | T_MINUS _ -> "T_MINUS"
    | T_UPLUS _ -> "T_UPLUS"
    | T_PLUS _ -> "T_PLUS"
    | T_COMMA _ -> "T_COMMA"
    | T_DOT _ -> "T_DOT"
end

type ('arg,'arg_comma_list_trail,'arg_comma_nonempty_list,'arg_comma_star_list,'array_body,'array_body_rest,'array_item,'assign_op,'assignable,'bin_op,'body_exn,'brace_codeblock,'call_args,'case_else,'class_inheritance,'code_block,'code_block_body,'command,'command_codeblock,'command_name,'constant,'do_codeblock,'do_sep,'ensure_clause,'eol_or_semi,'eols,'expr,'formal_arg,'formal_arg_list,'formal_arg_nonempty_list,'func,'identifier,'if_else_clauses,'interp_code,'interp_str,'interp_str_work,'keyword_as_id,'lhs,'lhs_assign_op,'lhs_prune_binop,'lhs_pruned_assign_op,'lparen,'message_identifier,'meth_or_atom,'meth_or_atom_list,'method_formals,'method_name,'mlhs,'mlhs_assign_op,'mlhs_clean,'mlhs_item,'mlhs_rest,'mrhs,'one_string,'primary,'rescue_clause,'rescue_list,'rescue_list_rest,'scope_begin,'scope_class,'scope_def,'scope_end,'scope_module,'scoped_id,'seen_id,'some_eols,'star_amper,'stmt,'stmt_list,'string,'then_sep,'topcall,'unary_op,'when_clauses) obj =
  | Obj_K_ALIAS of (Lexing.position)
  | Obj_K_AND of (Lexing.position)
  | Obj_K_BEGIN of (Lexing.position)
  | Obj_K_CASE of (Lexing.position)
  | Obj_K_CLASS of (string*Lexing.position)
  | Obj_K_DEF of (string*Lexing.position)
  | Obj_K_DO of (Lexing.position)
  | Obj_K_ELSE of (Lexing.position)
  | Obj_K_ELSIF of (Lexing.position)
  | Obj_K_END of (Lexing.position)
  | Obj_K_ENSURE of (Lexing.position)
  | Obj_K_FALSE of (Lexing.position)
  | Obj_K_FOR of (Lexing.position)
  | Obj_K_IF of (Lexing.position)
  | Obj_K_IN of (Lexing.position)
  | Obj_K_MODULE of (string*Lexing.position)
  | Obj_K_NIL of (Lexing.position)
  | Obj_K_NOT of (Lexing.position)
  | Obj_K_OR of (Lexing.position)
  | Obj_K_RESCUE of (Lexing.position)
  | Obj_K_RETURN of (Lexing.position)
  | Obj_K_SELF of (Lexing.position)
  | Obj_K_THEN of (Lexing.position)
  | Obj_K_TRUE of (Lexing.position)
  | Obj_K_UNDEF of (Lexing.position)
  | Obj_K_UNLESS of (Lexing.position)
  | Obj_K_UNTIL of (Lexing.position)
  | Obj_K_WHEN of (Lexing.position)
  | Obj_K_WHILE of (Lexing.position)
  | Obj_K_YIELD of (Lexing.position)
  | Obj_K_lBEGIN of (Lexing.position)
  | Obj_K_lEND of (Lexing.position)
  | Obj_T_AMPER of (Lexing.position)
  | Obj_T_ANDOP of (Lexing.position)
  | Obj_T_ASSIGN of (Lexing.position)
  | Obj_T_ASSOC of (Lexing.position)
  | Obj_T_ATOM of (Ast.interp_string * Lexing.position)
  | Obj_T_ATOM_BEG of (Lexing.position)
  | Obj_T_BANG of (Lexing.position)
  | Obj_T_BIGNUM of (Big_int.big_int * Lexing.position)
  | Obj_T_BUILTIN_VAR of (string * Lexing.position)
  | Obj_T_CARROT of (Lexing.position)
  | Obj_T_CAST of (string * string * Lexing.position)
  | Obj_T_CLASS_VAR of (string * Lexing.position)
  | Obj_T_CMP of (Lexing.position)
  | Obj_T_COLON of (Lexing.position)
  | Obj_T_COMMA of (Lexing.position)
  | Obj_T_DOT of (Lexing.position)
  | Obj_T_DOT2 of (Lexing.position)
  | Obj_T_DOT3 of (Lexing.position)
  | Obj_T_DOUBLE_BEG of (Lexing.position)
  | Obj_T_DOUBLE_STRING of (Ast.interp_string * Lexing.position)
  | Obj_T_EOF
  | Obj_T_EOL
  | Obj_T_EQ of (Lexing.position)
  | Obj_T_EQQ of (Lexing.position)
  | Obj_T_FIXNUM of (int * Lexing.position)
  | Obj_T_FLOAT of (string * float * Lexing.position)
  | Obj_T_GEQ of (Lexing.position)
  | Obj_T_GLOBAL_VAR of (string * Lexing.position)
  | Obj_T_GT of (Lexing.position)
  | Obj_T_INST_VAR of (string * Lexing.position)
  | Obj_T_INTERP_END of (string * Lexing.position)
  | Obj_T_INTERP_STR of (string * Lexing.position)
  | Obj_T_LBRACE of (Lexing.position)
  | Obj_T_LBRACE_ARG of (Lexing.position)
  | Obj_T_LBRACK of (Lexing.position)
  | Obj_T_LBRACK_ARG of (Lexing.position)
  | Obj_T_LEQ of (Lexing.position)
  | Obj_T_LID of (string * Lexing.position)
  | Obj_T_LPAREN of (Lexing.position)
  | Obj_T_LPAREN_ARG of (Lexing.position)
  | Obj_T_LSHFT of (Lexing.position)
  | Obj_T_LT of (Lexing.position)
  | Obj_T_MATCH of (Lexing.position)
  | Obj_T_MINUS of (Lexing.position)
  | Obj_T_NEQ of (Lexing.position)
  | Obj_T_NMATCH of (Lexing.position)
  | Obj_T_OP_ASGN of (string*Lexing.position)
  | Obj_T_OROP of (Lexing.position)
  | Obj_T_PERCENT of (Lexing.position)
  | Obj_T_PLUS of (Lexing.position)
  | Obj_T_POW of (Lexing.position)
  | Obj_T_QUESTION of (Lexing.position)
  | Obj_T_RBRACE of (Lexing.position)
  | Obj_T_RBRACK of (Lexing.position)
  | Obj_T_REGEXP of (Ast.interp_string * string * Lexing.position)
  | Obj_T_REGEXP_BEG of (Lexing.position)
  | Obj_T_REGEXP_MOD of (string)
  | Obj_T_RPAREN of (Lexing.position)
  | Obj_T_RSHFT of (Lexing.position)
  | Obj_T_SCOPE of (Lexing.position)
  | Obj_T_SEMICOLON of (Lexing.position)
  | Obj_T_SINGLE_STRING of (string * Lexing.position)
  | Obj_T_SLASH of (Lexing.position)
  | Obj_T_STAR of (Lexing.position)
  | Obj_T_TICK_BEG of (Lexing.position)
  | Obj_T_TILDE of (Lexing.position)
  | Obj_T_UAMPER of (Lexing.position)
  | Obj_T_UID of (string * Lexing.position)
  | Obj_T_UMINUS of (Lexing.position)
  | Obj_T_UPLUS of (Lexing.position)
  | Obj_T_USCOPE of (Lexing.position)
  | Obj_T_USER_BEG of (string * Lexing.position)
  | Obj_T_USER_STRING of (string * Ast.interp_string * Lexing.position)
  | Obj_T_USTAR of (Lexing.position)
  | Obj_T_VBAR of (Lexing.position)
  | Obj_arg of 'arg
  | Obj_arg_comma_list_trail of 'arg_comma_list_trail
  | Obj_arg_comma_nonempty_list of 'arg_comma_nonempty_list
  | Obj_arg_comma_star_list of 'arg_comma_star_list
  | Obj_array_body of 'array_body
  | Obj_array_body_rest of 'array_body_rest
  | Obj_array_item of 'array_item
  | Obj_assign_op of 'assign_op
  | Obj_assignable of 'assignable
  | Obj_bin_op of 'bin_op
  | Obj_body_exn of 'body_exn
  | Obj_brace_codeblock of 'brace_codeblock
  | Obj_call_args of 'call_args
  | Obj_case_else of 'case_else
  | Obj_class_inheritance of 'class_inheritance
  | Obj_code_block of 'code_block
  | Obj_code_block_body of 'code_block_body
  | Obj_command of 'command
  | Obj_command_codeblock of 'command_codeblock
  | Obj_command_name of 'command_name
  | Obj_constant of 'constant
  | Obj_do_codeblock of 'do_codeblock
  | Obj_do_sep of 'do_sep
  | Obj_ensure_clause of 'ensure_clause
  | Obj_eol_or_semi of 'eol_or_semi
  | Obj_eols of 'eols
  | Obj_expr of 'expr
  | Obj_formal_arg of 'formal_arg
  | Obj_formal_arg_list of 'formal_arg_list
  | Obj_formal_arg_nonempty_list of 'formal_arg_nonempty_list
  | Obj_func of 'func
  | Obj_identifier of 'identifier
  | Obj_if_else_clauses of 'if_else_clauses
  | Obj_interp_code of 'interp_code
  | Obj_interp_str of 'interp_str
  | Obj_interp_str_work of 'interp_str_work
  | Obj_keyword_as_id of 'keyword_as_id
  | Obj_lhs of 'lhs
  | Obj_lhs_assign_op of 'lhs_assign_op
  | Obj_lhs_prune_binop of 'lhs_prune_binop
  | Obj_lhs_pruned_assign_op of 'lhs_pruned_assign_op
  | Obj_lparen of 'lparen
  | Obj_main of (Ast.expr list)
  | Obj_message_identifier of 'message_identifier
  | Obj_meth_or_atom of 'meth_or_atom
  | Obj_meth_or_atom_list of 'meth_or_atom_list
  | Obj_method_formals of 'method_formals
  | Obj_method_name of 'method_name
  | Obj_mlhs of 'mlhs
  | Obj_mlhs_assign_op of 'mlhs_assign_op
  | Obj_mlhs_clean of 'mlhs_clean
  | Obj_mlhs_item of 'mlhs_item
  | Obj_mlhs_rest of 'mlhs_rest
  | Obj_mrhs of 'mrhs
  | Obj_one_string of 'one_string
  | Obj_primary of 'primary
  | Obj_rescue_clause of 'rescue_clause
  | Obj_rescue_list of 'rescue_list
  | Obj_rescue_list_rest of 'rescue_list_rest
  | Obj_scope_begin of 'scope_begin
  | Obj_scope_class of 'scope_class
  | Obj_scope_def of 'scope_def
  | Obj_scope_end of 'scope_end
  | Obj_scope_module of 'scope_module
  | Obj_scoped_id of 'scoped_id
  | Obj_seen_id of 'seen_id
  | Obj_some_eols of 'some_eols
  | Obj_star_amper of 'star_amper
  | Obj_stmt of 'stmt
  | Obj_stmt_list of 'stmt_list
  | Obj_string of 'string
  | Obj_then_sep of 'then_sep
  | Obj_topcall of 'topcall
  | Obj_unary_op of 'unary_op
  | Obj_when_clauses of 'when_clauses

module Dyp_symbols_array =
struct
  let str_non_ter =
  [|"S'";
    "arg";
    "arg_comma_list_trail";
    "arg_comma_nonempty_list";
    "arg_comma_star_list";
    "array_body";
    "array_body_rest";
    "array_item";
    "assign_op";
    "assignable";
    "bin_op";
    "body_exn";
    "brace_codeblock";
    "call_args";
    "case_else";
    "class_inheritance";
    "code_block";
    "code_block_body";
    "command";
    "command_codeblock";
    "command_name";
    "constant";
    "do_codeblock";
    "do_sep";
    "ensure_clause";
    "eol_or_semi";
    "eols";
    "expr";
    "formal_arg";
    "formal_arg_list";
    "formal_arg_nonempty_list";
    "func";
    "identifier";
    "if_else_clauses";
    "interp_code";
    "interp_str";
    "interp_str_work";
    "keyword_as_id";
    "lhs";
    "lhs_assign_op";
    "lhs_prune_binop";
    "lhs_pruned_assign_op";
    "lparen";
    "main";
    "message_identifier";
    "meth_or_atom";
    "meth_or_atom_list";
    "method_formals";
    "method_name";
    "mlhs";
    "mlhs_assign_op";
    "mlhs_clean";
    "mlhs_item";
    "mlhs_rest";
    "mrhs";
    "one_string";
    "primary";
    "rescue_clause";
    "rescue_list";
    "rescue_list_rest";
    "scope_begin";
    "scope_class";
    "scope_def";
    "scope_end";
    "scope_module";
    "scoped_id";
    "seen_id";
    "some_eols";
    "star_amper";
    "stmt";
    "stmt_list";
    "string";
    "then_sep";
    "topcall";
    "unary_op";
    "when_clauses";|]
  let token_name_array =
    [|"token_epsilon";"dummy_token_main";"T_EOF";"T_EOL";"T_INTERP_END";"T_INTERP_STR";"T_ATOM_BEG";"T_REGEXP";"T_REGEXP_MOD";"T_REGEXP_BEG";"T_USER_BEG";"T_TICK_BEG";"T_DOUBLE_BEG";"T_USER_STRING";"T_DOUBLE_STRING";"T_SINGLE_STRING";"K_FALSE";"K_TRUE";"K_SELF";"K_YIELD";"K_NIL";"K_lEND";"K_lBEGIN";"K_NOT";"K_OR";"K_AND";"K_RETURN";"K_DO";"K_IN";"K_FOR";"K_UNTIL";"K_WHILE";"K_WHEN";"K_CASE";"K_ELSE";"K_ELSIF";"K_THEN";"K_UNLESS";"K_IF";"K_ENSURE";"K_RESCUE";"K_BEGIN";"K_UNDEF";"K_ALIAS";"K_END";"K_DEF";"K_MODULE";"K_CLASS";"T_BUILTIN_VAR";"T_FLOAT";"T_BIGNUM";"T_FIXNUM";"T_ATOM";"T_CLASS_VAR";"T_INST_VAR";"T_GLOBAL_VAR";"T_LID";"T_UID";"T_CAST";"T_USCOPE";"T_SCOPE";"T_SEMICOLON";"T_UAMPER";"T_AMPER";"T_TILDE";"T_BANG";"T_VBAR";"T_CARROT";"T_COLON";"T_QUESTION";"T_PERCENT";"T_SLASH";"T_USTAR";"T_STAR";"T_RBRACE";"T_LBRACE_ARG";"T_LBRACE";"T_RBRACK";"T_LBRACK";"T_LBRACK_ARG";"T_RPAREN";"T_LPAREN_ARG";"T_LPAREN";"T_ASSOC";"T_OP_ASGN";"T_RSHFT";"T_LSHFT";"T_DOT3";"T_DOT2";"T_NMATCH";"T_MATCH";"T_OROP";"T_ANDOP";"T_GT";"T_LT";"T_LEQ";"T_GEQ";"T_NEQ";"T_EQQ";"T_EQ";"T_ASSIGN";"T_CMP";"T_POW";"T_UMINUS";"T_MINUS";"T_UPLUS";"T_PLUS";"T_COMMA";"T_DOT"|]
  let test_cons =  [|
    (fun x -> match x with Obj_arg _ -> true | _ -> false);
    (fun x -> match x with Obj_arg_comma_list_trail _ -> true | _ -> false);
    (fun x -> match x with Obj_arg_comma_nonempty_list _ -> true | _ -> false);
    (fun x -> match x with Obj_arg_comma_star_list _ -> true | _ -> false);
    (fun x -> match x with Obj_array_body _ -> true | _ -> false);
    (fun x -> match x with Obj_array_body_rest _ -> true | _ -> false);
    (fun x -> match x with Obj_array_item _ -> true | _ -> false);
    (fun x -> match x with Obj_assign_op _ -> true | _ -> false);
    (fun x -> match x with Obj_assignable _ -> true | _ -> false);
    (fun x -> match x with Obj_bin_op _ -> true | _ -> false);
    (fun x -> match x with Obj_body_exn _ -> true | _ -> false);
    (fun x -> match x with Obj_brace_codeblock _ -> true | _ -> false);
    (fun x -> match x with Obj_call_args _ -> true | _ -> false);
    (fun x -> match x with Obj_case_else _ -> true | _ -> false);
    (fun x -> match x with Obj_class_inheritance _ -> true | _ -> false);
    (fun x -> match x with Obj_code_block _ -> true | _ -> false);
    (fun x -> match x with Obj_code_block_body _ -> true | _ -> false);
    (fun x -> match x with Obj_command _ -> true | _ -> false);
    (fun x -> match x with Obj_command_codeblock _ -> true | _ -> false);
    (fun x -> match x with Obj_command_name _ -> true | _ -> false);
    (fun x -> match x with Obj_constant _ -> true | _ -> false);
    (fun x -> match x with Obj_do_codeblock _ -> true | _ -> false);
    (fun x -> match x with Obj_do_sep _ -> true | _ -> false);
    (fun x -> match x with Obj_ensure_clause _ -> true | _ -> false);
    (fun x -> match x with Obj_eol_or_semi _ -> true | _ -> false);
    (fun x -> match x with Obj_eols _ -> true | _ -> false);
    (fun x -> match x with Obj_expr _ -> true | _ -> false);
    (fun x -> match x with Obj_formal_arg _ -> true | _ -> false);
    (fun x -> match x with Obj_formal_arg_list _ -> true | _ -> false);
    (fun x -> match x with Obj_formal_arg_nonempty_list _ -> true | _ -> false);
    (fun x -> match x with Obj_func _ -> true | _ -> false);
    (fun x -> match x with Obj_identifier _ -> true | _ -> false);
    (fun x -> match x with Obj_if_else_clauses _ -> true | _ -> false);
    (fun x -> match x with Obj_interp_code _ -> true | _ -> false);
    (fun x -> match x with Obj_interp_str _ -> true | _ -> false);
    (fun x -> match x with Obj_interp_str_work _ -> true | _ -> false);
    (fun x -> match x with Obj_keyword_as_id _ -> true | _ -> false);
    (fun x -> match x with Obj_lhs _ -> true | _ -> false);
    (fun x -> match x with Obj_lhs_assign_op _ -> true | _ -> false);
    (fun x -> match x with Obj_lhs_prune_binop _ -> true | _ -> false);
    (fun x -> match x with Obj_lhs_pruned_assign_op _ -> true | _ -> false);
    (fun x -> match x with Obj_lparen _ -> true | _ -> false);
    (fun x -> match x with Obj_main _ -> true | _ -> false);
    (fun x -> match x with Obj_message_identifier _ -> true | _ -> false);
    (fun x -> match x with Obj_meth_or_atom _ -> true | _ -> false);
    (fun x -> match x with Obj_meth_or_atom_list _ -> true | _ -> false);
    (fun x -> match x with Obj_method_formals _ -> true | _ -> false);
    (fun x -> match x with Obj_method_name _ -> true | _ -> false);
    (fun x -> match x with Obj_mlhs _ -> true | _ -> false);
    (fun x -> match x with Obj_mlhs_assign_op _ -> true | _ -> false);
    (fun x -> match x with Obj_mlhs_clean _ -> true | _ -> false);
    (fun x -> match x with Obj_mlhs_item _ -> true | _ -> false);
    (fun x -> match x with Obj_mlhs_rest _ -> true | _ -> false);
    (fun x -> match x with Obj_mrhs _ -> true | _ -> false);
    (fun x -> match x with Obj_one_string _ -> true | _ -> false);
    (fun x -> match x with Obj_primary _ -> true | _ -> false);
    (fun x -> match x with Obj_rescue_clause _ -> true | _ -> false);
    (fun x -> match x with Obj_rescue_list _ -> true | _ -> false);
    (fun x -> match x with Obj_rescue_list_rest _ -> true | _ -> false);
    (fun x -> match x with Obj_scope_begin _ -> true | _ -> false);
    (fun x -> match x with Obj_scope_class _ -> true | _ -> false);
    (fun x -> match x with Obj_scope_def _ -> true | _ -> false);
    (fun x -> match x with Obj_scope_end _ -> true | _ -> false);
    (fun x -> match x with Obj_scope_module _ -> true | _ -> false);
    (fun x -> match x with Obj_scoped_id _ -> true | _ -> false);
    (fun x -> match x with Obj_seen_id _ -> true | _ -> false);
    (fun x -> match x with Obj_some_eols _ -> true | _ -> false);
    (fun x -> match x with Obj_star_amper _ -> true | _ -> false);
    (fun x -> match x with Obj_stmt _ -> true | _ -> false);
    (fun x -> match x with Obj_stmt_list _ -> true | _ -> false);
    (fun x -> match x with Obj_string _ -> true | _ -> false);
    (fun x -> match x with Obj_then_sep _ -> true | _ -> false);
    (fun x -> match x with Obj_topcall _ -> true | _ -> false);
    (fun x -> match x with Obj_unary_op _ -> true | _ -> false);
    (fun x -> match x with Obj_when_clauses _ -> true | _ -> false)|]
  let cons_of_nt =
  [|0;
    0;
    1;
    2;
    3;
    4;
    5;
    6;
    7;
    8;
    9;
    10;
    11;
    12;
    13;
    14;
    15;
    16;
    17;
    18;
    19;
    20;
    21;
    22;
    23;
    24;
    25;
    26;
    27;
    28;
    29;
    30;
    31;
    32;
    33;
    34;
    35;
    36;
    37;
    38;
    39;
    40;
    41;
    42;
    43;
    44;
    45;
    46;
    47;
    48;
    49;
    50;
    51;
    52;
    53;
    54;
    55;
    56;
    57;
    58;
    59;
    60;
    61;
    62;
    63;
    64;
    65;
    66;
    67;
    68;
    69;
    70;
    71;
    72;
    73;
    74|]
  let str_cons o = match o with
    | Obj_arg _ -> "Obj_arg"
    | Obj_arg_comma_list_trail _ -> "Obj_arg_comma_list_trail"
    | Obj_arg_comma_nonempty_list _ -> "Obj_arg_comma_nonempty_list"
    | Obj_arg_comma_star_list _ -> "Obj_arg_comma_star_list"
    | Obj_array_body _ -> "Obj_array_body"
    | Obj_array_body_rest _ -> "Obj_array_body_rest"
    | Obj_array_item _ -> "Obj_array_item"
    | Obj_assign_op _ -> "Obj_assign_op"
    | Obj_assignable _ -> "Obj_assignable"
    | Obj_bin_op _ -> "Obj_bin_op"
    | Obj_body_exn _ -> "Obj_body_exn"
    | Obj_brace_codeblock _ -> "Obj_brace_codeblock"
    | Obj_call_args _ -> "Obj_call_args"
    | Obj_case_else _ -> "Obj_case_else"
    | Obj_class_inheritance _ -> "Obj_class_inheritance"
    | Obj_code_block _ -> "Obj_code_block"
    | Obj_code_block_body _ -> "Obj_code_block_body"
    | Obj_command _ -> "Obj_command"
    | Obj_command_codeblock _ -> "Obj_command_codeblock"
    | Obj_command_name _ -> "Obj_command_name"
    | Obj_constant _ -> "Obj_constant"
    | Obj_do_codeblock _ -> "Obj_do_codeblock"
    | Obj_do_sep _ -> "Obj_do_sep"
    | Obj_ensure_clause _ -> "Obj_ensure_clause"
    | Obj_eol_or_semi _ -> "Obj_eol_or_semi"
    | Obj_eols _ -> "Obj_eols"
    | Obj_expr _ -> "Obj_expr"
    | Obj_formal_arg _ -> "Obj_formal_arg"
    | Obj_formal_arg_list _ -> "Obj_formal_arg_list"
    | Obj_formal_arg_nonempty_list _ -> "Obj_formal_arg_nonempty_list"
    | Obj_func _ -> "Obj_func"
    | Obj_identifier _ -> "Obj_identifier"
    | Obj_if_else_clauses _ -> "Obj_if_else_clauses"
    | Obj_interp_code _ -> "Obj_interp_code"
    | Obj_interp_str _ -> "Obj_interp_str"
    | Obj_interp_str_work _ -> "Obj_interp_str_work"
    | Obj_keyword_as_id _ -> "Obj_keyword_as_id"
    | Obj_lhs _ -> "Obj_lhs"
    | Obj_lhs_assign_op _ -> "Obj_lhs_assign_op"
    | Obj_lhs_prune_binop _ -> "Obj_lhs_prune_binop"
    | Obj_lhs_pruned_assign_op _ -> "Obj_lhs_pruned_assign_op"
    | Obj_lparen _ -> "Obj_lparen"
    | Obj_main _ -> "Obj_main"
    | Obj_message_identifier _ -> "Obj_message_identifier"
    | Obj_meth_or_atom _ -> "Obj_meth_or_atom"
    | Obj_meth_or_atom_list _ -> "Obj_meth_or_atom_list"
    | Obj_method_formals _ -> "Obj_method_formals"
    | Obj_method_name _ -> "Obj_method_name"
    | Obj_mlhs _ -> "Obj_mlhs"
    | Obj_mlhs_assign_op _ -> "Obj_mlhs_assign_op"
    | Obj_mlhs_clean _ -> "Obj_mlhs_clean"
    | Obj_mlhs_item _ -> "Obj_mlhs_item"
    | Obj_mlhs_rest _ -> "Obj_mlhs_rest"
    | Obj_mrhs _ -> "Obj_mrhs"
    | Obj_one_string _ -> "Obj_one_string"
    | Obj_primary _ -> "Obj_primary"
    | Obj_rescue_clause _ -> "Obj_rescue_clause"
    | Obj_rescue_list _ -> "Obj_rescue_list"
    | Obj_rescue_list_rest _ -> "Obj_rescue_list_rest"
    | Obj_scope_begin _ -> "Obj_scope_begin"
    | Obj_scope_class _ -> "Obj_scope_class"
    | Obj_scope_def _ -> "Obj_scope_def"
    | Obj_scope_end _ -> "Obj_scope_end"
    | Obj_scope_module _ -> "Obj_scope_module"
    | Obj_scoped_id _ -> "Obj_scoped_id"
    | Obj_seen_id _ -> "Obj_seen_id"
    | Obj_some_eols _ -> "Obj_some_eols"
    | Obj_star_amper _ -> "Obj_star_amper"
    | Obj_stmt _ -> "Obj_stmt"
    | Obj_stmt_list _ -> "Obj_stmt_list"
    | Obj_string _ -> "Obj_string"
    | Obj_then_sep _ -> "Obj_then_sep"
    | Obj_topcall _ -> "Obj_topcall"
    | Obj_unary_op _ -> "Obj_unary_op"
    | Obj_when_clauses _ -> "Obj_when_clauses"
    | _ -> failwith "str_cons, unexpected constructor"
end

module Dyp_parameters =
struct
  let token_nb = 109
  let undef_nt = true
  let entry_points = [(Dyp_symbols.main,1)]
  let str_token_name t = Dyp_symbols_array.token_name_array.(t)
  let priority_names = [|"default_priority"|]
  let merge_warning = false
end

module Dyp_runtime = Dyp.Make_dyp(Dyp_parameters)
module Dyp_engine = Dyp_runtime.Parser_PIA

module Dyp_aux_functions =
struct
  let datadyn = Dyp_runtime.Tools.init_datadyn
["arg",0,"Obj_arg";"arg_comma_list_trail",1,"Obj_arg_comma_list_trail";"arg_comma_nonempty_list",2,"Obj_arg_comma_nonempty_list";"arg_comma_star_list",3,"Obj_arg_comma_star_list";"array_body",4,"Obj_array_body";"array_body_rest",5,"Obj_array_body_rest";"array_item",6,"Obj_array_item";"assign_op",7,"Obj_assign_op";"assignable",8,"Obj_assignable";"bin_op",9,"Obj_bin_op";"body_exn",10,"Obj_body_exn";"brace_codeblock",11,"Obj_brace_codeblock";"call_args",12,"Obj_call_args";"case_else",13,"Obj_case_else";"class_inheritance",14,"Obj_class_inheritance";"code_block",15,"Obj_code_block";"code_block_body",16,"Obj_code_block_body";"command",17,"Obj_command";"command_codeblock",18,"Obj_command_codeblock";"command_name",19,"Obj_command_name";"constant",20,"Obj_constant";"do_codeblock",21,"Obj_do_codeblock";"do_sep",22,"Obj_do_sep";"ensure_clause",23,"Obj_ensure_clause";"eol_or_semi",24,"Obj_eol_or_semi";"eols",25,"Obj_eols";"expr",26,"Obj_expr";"formal_arg",27,"Obj_formal_arg";"formal_arg_list",28,"Obj_formal_arg_list";"formal_arg_nonempty_list",29,"Obj_formal_arg_nonempty_list";"func",30,"Obj_func";"identifier",31,"Obj_identifier";"if_else_clauses",32,"Obj_if_else_clauses";"interp_code",33,"Obj_interp_code";"interp_str",34,"Obj_interp_str";"interp_str_work",35,"Obj_interp_str_work";"keyword_as_id",36,"Obj_keyword_as_id";"lhs",37,"Obj_lhs";"lhs_assign_op",38,"Obj_lhs_assign_op";"lhs_prune_binop",39,"Obj_lhs_prune_binop";"lhs_pruned_assign_op",40,"Obj_lhs_pruned_assign_op";"lparen",41,"Obj_lparen";"main",42,"Obj_main";"message_identifier",43,"Obj_message_identifier";"meth_or_atom",44,"Obj_meth_or_atom";"meth_or_atom_list",45,"Obj_meth_or_atom_list";"method_formals",46,"Obj_method_formals";"method_name",47,"Obj_method_name";"mlhs",48,"Obj_mlhs";"mlhs_assign_op",49,"Obj_mlhs_assign_op";"mlhs_clean",50,"Obj_mlhs_clean";"mlhs_item",51,"Obj_mlhs_item";"mlhs_rest",52,"Obj_mlhs_rest";"mrhs",53,"Obj_mrhs";"one_string",54,"Obj_one_string";"primary",55,"Obj_primary";"rescue_clause",56,"Obj_rescue_clause";"rescue_list",57,"Obj_rescue_list";"rescue_list_rest",58,"Obj_rescue_list_rest";"scope_begin",59,"Obj_scope_begin";"scope_class",60,"Obj_scope_class";"scope_def",61,"Obj_scope_def";"scope_end",62,"Obj_scope_end";"scope_module",63,"Obj_scope_module";"scoped_id",64,"Obj_scoped_id";"seen_id",65,"Obj_seen_id";"some_eols",66,"Obj_some_eols";"star_amper",67,"Obj_star_amper";"stmt",68,"Obj_stmt";"stmt_list",69,"Obj_stmt_list";"string",70,"Obj_string";"then_sep",71,"Obj_then_sep";"topcall",72,"Obj_topcall";"unary_op",73,"Obj_unary_op";"when_clauses",74,"Obj_when_clauses"]
["Obj_arg";"Obj_arg_comma_list_trail";"Obj_arg_comma_nonempty_list";"Obj_arg_comma_star_list";"Obj_array_body";"Obj_array_body_rest";"Obj_array_item";"Obj_assign_op";"Obj_assignable";"Obj_bin_op";"Obj_body_exn";"Obj_brace_codeblock";"Obj_call_args";"Obj_case_else";"Obj_class_inheritance";"Obj_code_block";"Obj_code_block_body";"Obj_command";"Obj_command_codeblock";"Obj_command_name";"Obj_constant";"Obj_do_codeblock";"Obj_do_sep";"Obj_ensure_clause";"Obj_eol_or_semi";"Obj_eols";"Obj_expr";"Obj_formal_arg";"Obj_formal_arg_list";"Obj_formal_arg_nonempty_list";"Obj_func";"Obj_identifier";"Obj_if_else_clauses";"Obj_interp_code";"Obj_interp_str";"Obj_interp_str_work";"Obj_keyword_as_id";"Obj_lhs";"Obj_lhs_assign_op";"Obj_lhs_prune_binop";"Obj_lhs_pruned_assign_op";"Obj_lparen";"Obj_main";"Obj_message_identifier";"Obj_meth_or_atom";"Obj_meth_or_atom_list";"Obj_method_formals";"Obj_method_name";"Obj_mlhs";"Obj_mlhs_assign_op";"Obj_mlhs_clean";"Obj_mlhs_item";"Obj_mlhs_rest";"Obj_mrhs";"Obj_one_string";"Obj_primary";"Obj_rescue_clause";"Obj_rescue_list";"Obj_rescue_list_rest";"Obj_scope_begin";"Obj_scope_class";"Obj_scope_def";"Obj_scope_end";"Obj_scope_module";"Obj_scoped_id";"Obj_seen_id";"Obj_some_eols";"Obj_star_amper";"Obj_stmt";"Obj_stmt_list";"Obj_string";"Obj_then_sep";"Obj_topcall";"Obj_unary_op";"Obj_when_clauses"]
  let get_token_value t = match t with
    | T_EOF -> Obj_T_EOF
    | T_EOL -> Obj_T_EOL
    | T_INTERP_END x -> Obj_T_INTERP_END x
    | T_INTERP_STR x -> Obj_T_INTERP_STR x
    | T_ATOM_BEG x -> Obj_T_ATOM_BEG x
    | T_REGEXP x -> Obj_T_REGEXP x
    | T_REGEXP_MOD x -> Obj_T_REGEXP_MOD x
    | T_REGEXP_BEG x -> Obj_T_REGEXP_BEG x
    | T_USER_BEG x -> Obj_T_USER_BEG x
    | T_TICK_BEG x -> Obj_T_TICK_BEG x
    | T_DOUBLE_BEG x -> Obj_T_DOUBLE_BEG x
    | T_USER_STRING x -> Obj_T_USER_STRING x
    | T_DOUBLE_STRING x -> Obj_T_DOUBLE_STRING x
    | T_SINGLE_STRING x -> Obj_T_SINGLE_STRING x
    | K_FALSE x -> Obj_K_FALSE x
    | K_TRUE x -> Obj_K_TRUE x
    | K_SELF x -> Obj_K_SELF x
    | K_YIELD x -> Obj_K_YIELD x
    | K_NIL x -> Obj_K_NIL x
    | K_lEND x -> Obj_K_lEND x
    | K_lBEGIN x -> Obj_K_lBEGIN x
    | K_NOT x -> Obj_K_NOT x
    | K_OR x -> Obj_K_OR x
    | K_AND x -> Obj_K_AND x
    | K_RETURN x -> Obj_K_RETURN x
    | K_DO x -> Obj_K_DO x
    | K_IN x -> Obj_K_IN x
    | K_FOR x -> Obj_K_FOR x
    | K_UNTIL x -> Obj_K_UNTIL x
    | K_WHILE x -> Obj_K_WHILE x
    | K_WHEN x -> Obj_K_WHEN x
    | K_CASE x -> Obj_K_CASE x
    | K_ELSE x -> Obj_K_ELSE x
    | K_ELSIF x -> Obj_K_ELSIF x
    | K_THEN x -> Obj_K_THEN x
    | K_UNLESS x -> Obj_K_UNLESS x
    | K_IF x -> Obj_K_IF x
    | K_ENSURE x -> Obj_K_ENSURE x
    | K_RESCUE x -> Obj_K_RESCUE x
    | K_BEGIN x -> Obj_K_BEGIN x
    | K_UNDEF x -> Obj_K_UNDEF x
    | K_ALIAS x -> Obj_K_ALIAS x
    | K_END x -> Obj_K_END x
    | K_DEF x -> Obj_K_DEF x
    | K_MODULE x -> Obj_K_MODULE x
    | K_CLASS x -> Obj_K_CLASS x
    | T_BUILTIN_VAR x -> Obj_T_BUILTIN_VAR x
    | T_FLOAT x -> Obj_T_FLOAT x
    | T_BIGNUM x -> Obj_T_BIGNUM x
    | T_FIXNUM x -> Obj_T_FIXNUM x
    | T_ATOM x -> Obj_T_ATOM x
    | T_CLASS_VAR x -> Obj_T_CLASS_VAR x
    | T_INST_VAR x -> Obj_T_INST_VAR x
    | T_GLOBAL_VAR x -> Obj_T_GLOBAL_VAR x
    | T_LID x -> Obj_T_LID x
    | T_UID x -> Obj_T_UID x
    | T_CAST x -> Obj_T_CAST x
    | T_USCOPE x -> Obj_T_USCOPE x
    | T_SCOPE x -> Obj_T_SCOPE x
    | T_SEMICOLON x -> Obj_T_SEMICOLON x
    | T_UAMPER x -> Obj_T_UAMPER x
    | T_AMPER x -> Obj_T_AMPER x
    | T_TILDE x -> Obj_T_TILDE x
    | T_BANG x -> Obj_T_BANG x
    | T_VBAR x -> Obj_T_VBAR x
    | T_CARROT x -> Obj_T_CARROT x
    | T_COLON x -> Obj_T_COLON x
    | T_QUESTION x -> Obj_T_QUESTION x
    | T_PERCENT x -> Obj_T_PERCENT x
    | T_SLASH x -> Obj_T_SLASH x
    | T_USTAR x -> Obj_T_USTAR x
    | T_STAR x -> Obj_T_STAR x
    | T_RBRACE x -> Obj_T_RBRACE x
    | T_LBRACE_ARG x -> Obj_T_LBRACE_ARG x
    | T_LBRACE x -> Obj_T_LBRACE x
    | T_RBRACK x -> Obj_T_RBRACK x
    | T_LBRACK x -> Obj_T_LBRACK x
    | T_LBRACK_ARG x -> Obj_T_LBRACK_ARG x
    | T_RPAREN x -> Obj_T_RPAREN x
    | T_LPAREN_ARG x -> Obj_T_LPAREN_ARG x
    | T_LPAREN x -> Obj_T_LPAREN x
    | T_ASSOC x -> Obj_T_ASSOC x
    | T_OP_ASGN x -> Obj_T_OP_ASGN x
    | T_RSHFT x -> Obj_T_RSHFT x
    | T_LSHFT x -> Obj_T_LSHFT x
    | T_DOT3 x -> Obj_T_DOT3 x
    | T_DOT2 x -> Obj_T_DOT2 x
    | T_NMATCH x -> Obj_T_NMATCH x
    | T_MATCH x -> Obj_T_MATCH x
    | T_OROP x -> Obj_T_OROP x
    | T_ANDOP x -> Obj_T_ANDOP x
    | T_GT x -> Obj_T_GT x
    | T_LT x -> Obj_T_LT x
    | T_LEQ x -> Obj_T_LEQ x
    | T_GEQ x -> Obj_T_GEQ x
    | T_NEQ x -> Obj_T_NEQ x
    | T_EQQ x -> Obj_T_EQQ x
    | T_EQ x -> Obj_T_EQ x
    | T_ASSIGN x -> Obj_T_ASSIGN x
    | T_CMP x -> Obj_T_CMP x
    | T_POW x -> Obj_T_POW x
    | T_UMINUS x -> Obj_T_UMINUS x
    | T_MINUS x -> Obj_T_MINUS x
    | T_UPLUS x -> Obj_T_UPLUS x
    | T_PLUS x -> Obj_T_PLUS x
    | T_COMMA x -> Obj_T_COMMA x
    | T_DOT x -> Obj_T_DOT x
  let lexbuf_position lexbuf = (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)
  let transform_av_list l =
    let f o = match o with
      | Obj_T_EOF -> `Dummy_obj
      | Obj_T_EOL -> `Dummy_obj
      | x -> `Real_obj x
    in
    List.map f l
end

module Dyp_priority_data =
struct
  let priority_data, default_priority =
    Dyp.insert_priority Dyp.empty_priority_data "default_priority"
end

let global_data = ref 0
let local_data = ref 0
let global_data_equal = (==)
let local_data_equal = (==)

let dyp_merge_arg _ _ = []
let dyp_merge_arg_comma_list_trail _ _ = []
let dyp_merge_arg_comma_nonempty_list _ _ = []
let dyp_merge_arg_comma_star_list _ _ = []
let dyp_merge_array_body _ _ = []
let dyp_merge_array_body_rest _ _ = []
let dyp_merge_array_item _ _ = []
let dyp_merge_assign_op _ _ = []
let dyp_merge_assignable _ _ = []
let dyp_merge_bin_op _ _ = []
let dyp_merge_body_exn _ _ = []
let dyp_merge_brace_codeblock _ _ = []
let dyp_merge_call_args _ _ = []
let dyp_merge_case_else _ _ = []
let dyp_merge_class_inheritance _ _ = []
let dyp_merge_code_block _ _ = []
let dyp_merge_code_block_body _ _ = []
let dyp_merge_command _ _ = []
let dyp_merge_command_codeblock _ _ = []
let dyp_merge_command_name _ _ = []
let dyp_merge_constant _ _ = []
let dyp_merge_do_codeblock _ _ = []
let dyp_merge_do_sep _ _ = []
let dyp_merge_ensure_clause _ _ = []
let dyp_merge_eol_or_semi _ _ = []
let dyp_merge_eols _ _ = []
let dyp_merge_expr _ _ = []
let dyp_merge_formal_arg _ _ = []
let dyp_merge_formal_arg_list _ _ = []
let dyp_merge_formal_arg_nonempty_list _ _ = []
let dyp_merge_func _ _ = []
let dyp_merge_identifier _ _ = []
let dyp_merge_if_else_clauses _ _ = []
let dyp_merge_interp_code _ _ = []
let dyp_merge_interp_str _ _ = []
let dyp_merge_interp_str_work _ _ = []
let dyp_merge_keyword_as_id _ _ = []
let dyp_merge_lhs _ _ = []
let dyp_merge_lhs_assign_op _ _ = []
let dyp_merge_lhs_prune_binop _ _ = []
let dyp_merge_lhs_pruned_assign_op _ _ = []
let dyp_merge_lparen _ _ = []
let dyp_merge_main _ _ = []
let dyp_merge_message_identifier _ _ = []
let dyp_merge_meth_or_atom _ _ = []
let dyp_merge_meth_or_atom_list _ _ = []
let dyp_merge_method_formals _ _ = []
let dyp_merge_method_name _ _ = []
let dyp_merge_mlhs _ _ = []
let dyp_merge_mlhs_assign_op _ _ = []
let dyp_merge_mlhs_clean _ _ = []
let dyp_merge_mlhs_item _ _ = []
let dyp_merge_mlhs_rest _ _ = []
let dyp_merge_mrhs _ _ = []
let dyp_merge_one_string _ _ = []
let dyp_merge_primary _ _ = []
let dyp_merge_rescue_clause _ _ = []
let dyp_merge_rescue_list _ _ = []
let dyp_merge_rescue_list_rest _ _ = []
let dyp_merge_scope_begin _ _ = []
let dyp_merge_scope_class _ _ = []
let dyp_merge_scope_def _ _ = []
let dyp_merge_scope_end _ _ = []
let dyp_merge_scope_module _ _ = []
let dyp_merge_scoped_id _ _ = []
let dyp_merge_seen_id _ _ = []
let dyp_merge_some_eols _ _ = []
let dyp_merge_star_amper _ _ = []
let dyp_merge_stmt _ _ = []
let dyp_merge_stmt_list _ _ = []
let dyp_merge_string _ _ = []
let dyp_merge_then_sep _ _ = []
let dyp_merge_topcall _ _ = []
let dyp_merge_unary_op _ _ = []
let dyp_merge_when_clauses _ _ = []
let dyp_merge = Dyp.keep_oldest

# 2 "newParser.dyp"

  open Ast
    
  module Env = Utils.StrSet

  let env_stack = 
    let s = Stack.create () in
      Stack.push Env.empty s;
      s
    
  let enter_scope dyp = 
    Stack.push Env.empty env_stack

  let leave_scope dyp = 
    ignore(Stack.pop env_stack)

  let clear_env () = 
    Stack.clear env_stack;
    enter_scope ()

  let set_env new_env = 
    Stack.clear env_stack;
    Stack.push new_env env_stack
      
  let env () = Stack.top env_stack

  let assigned_id id = Env.mem id (env())

  let seen_str dyp id = 
    let env = Stack.pop env_stack in
      Stack.push (Env.add id env) env_stack

  let rec seen dyp = function
    | E_Identifier(ID_Lowercase,s,_) -> seen_str dyp s
    | E_Array(es,_) | E_Tuple(es,_) -> List.iter (seen dyp) es
    | _ -> ()

  let state_override = ref false
  let begin_override () =
    let b = !state_override in
      state_override := false;
      b

  let is_exnblock = function | E_ExnBlock _ -> true | _ -> false

  let mk_block lst pos = match lst with
    | [x] -> x
    | _ -> E_Block(lst,pos)

  let rec add_eq = function
    | E_Identifier(k,s,p) -> E_Identifier(ID_Assign(k), s,p)
    | E_Binop(e1,Op_SCOPE,e2,p) -> E_Binop(e1,Op_SCOPE, add_eq e2,p)
    | E_Binop(e1,Op_DOT,e2,p) -> E_Binop(e1,Op_DOT, add_eq e2,p)
    | E_Operator(Op_AREF,p) -> E_Operator(Op_ASET,p)
    | _ -> failwith "add_eq"

  (* turn %w{a b c} into ["a";"b";"c"]  *)
  let bslash_spc_re = Str.regexp "\\\\ " 
  let ws_re = Str.regexp "[ \t\r\n]+"
  let split_single_string_to_array str pos = 
    let chunks = 
      List.map (fun chunk -> Str.split ws_re chunk) 
        (Str.split_delim bslash_spc_re str)
    in
    let strings =
      let rec reduce acc chunks = match acc, chunks with
        | _, [] -> List.rev acc (* done *)
        | [], [chunk] -> chunk  (* no / found *)
        | [], []::chunks_t -> (* space in the front??? *)
            reduce [""] chunks_t
        | [], chunks_h::chunks_t -> (* other first iter *)
            reduce (List.rev chunks_h) chunks_t
        | acc_h::acc_t, []::chunks_t -> 
            reduce ((acc_h ^ " ")::acc_t) chunks_t
        | acc_h::acc_t, chunks_h::chunks_t -> 
            let first = List.hd chunks_h in
            let rest_rev = List.rev(List.tl chunks_h) in
              reduce (rest_rev @ ((acc_h ^ " " ^ first)::acc_t)) chunks_t
      in reduce [] chunks
    in
    let strings = List.map
      (fun s -> E_Literal(Lit_String(String_Single s), pos)) strings
    in
      E_Array(strings,pos)

  (* turn %W{a#{b} c} into ["a#{b}"; "c"] *) 
  let split_double_string_to_array sc pos =
    let ds s = E_Literal(Lit_String(String_Double s),pos) in
      (* first we create a stream of tokens with the grammar of
	   (Expr | Code | String | Delmi)*
	 by splitting the strings on whitespace.  This stream will be
	 in reverse order.
      *)
    let rec tokenize acc = function
      | [] -> acc
      | (Ast.StrExpr e)::tl -> tokenize ((`Expr e)::acc) tl
      | (Ast.StrChars s)::tl -> 
	  let splits = Str.full_split ws_re s in
	  let acc = 
	    List.fold_left
	      (fun acc -> function
		 | Str.Text s -> (`String s)::acc
		 | Str.Delim _ -> `Delim::acc
	      ) acc splits
	  in tokenize acc tl
    in
      (* then we split the (reversed) stream at the delimeters, which
	 mark the entries in the array.  This produces a list in the
	 correct order. *)
    let rec parse acc curr = function
      | [] -> 
	  if curr = [] 
	  then acc (* delim at end *)
	  else (ds curr)::acc
      | `Delim::tl -> 
	  if curr = [] then parse acc curr tl (* delim at start *)
	  else parse ((ds curr)::acc) [] tl
      | (`Expr s)::tl -> parse acc ((Ast.StrExpr s)::curr) tl
      | (`String s)::tl -> parse acc ((Ast.StrChars s)::curr) tl
    in
    let toks = tokenize [] sc in
    let lst = parse [] [] toks in
      E_Array(lst, pos)

  let str_of_interp sc = match sc with
    | []  -> ""
    | [Ast.StrChars s] -> s
    | _ -> failwith "unexpected escapes in string"

  let merge_string_lits s1 s2 = match s1,s2 with
    | E_Literal(Lit_String(s1),p), E_Literal(Lit_String(s2),_) ->
	let s' = match s1, s2 with
	  | String_Tick _, _ | _, String_Tick _ -> assert false
	  | String_Single s1, String_Single s2 -> String_Single (s1 ^ s2)
	  | String_Double sc1, String_Double sc2 -> String_Double (sc1 @ sc2)
	  | String_Single s, String_Double sc -> 
	      String_Double ((Ast.StrChars s)::sc)
	  | String_Double sc,String_Single s -> 
	      String_Double (sc @ [Ast.StrChars s])
	in
	  E_Literal((Lit_String s'),p)
    | _ -> assert false

  let process_user_string m str pos = match m with
    | "r" -> E_Literal(Lit_Regexp (str,""),pos)
    | "w" -> split_single_string_to_array (str_of_interp str) pos
    | "W" -> split_double_string_to_array str pos
    | "q" -> E_Literal(Lit_String(String_Single (str_of_interp str)),pos)
    | "Q" -> E_Literal(Lit_String(String_Double str),pos)
    | "x" -> E_Literal(Lit_String(String_Tick str),pos)
    | "" -> E_Literal(Lit_String(String_Double str),pos)
    | _ -> failwith (Printf.sprintf "unhandled string modifier: %s" m)

  let rec starts_with = function
    | E_Binop(l,_,_,_) -> starts_with l
    | E_MethodCall(l,_,_,_) -> starts_with l
    | E_Ternary(l,_,_,_) -> starts_with l
    | e -> e

  let rec ends_with = function
    | E_Binop(_,_,r,_) -> ends_with r
    | E_MethodCall(m,[],None,_) -> ends_with m
    | E_Ternary(_,_,r,_) -> ends_with r
    | e -> e
	
  let rec replace_end expr new_e = match expr with
    | E_Binop(l,o,r,p) -> E_Binop(l,o,replace_end r new_e,p)
    | E_MethodCall(m,[],None,p) -> E_MethodCall(replace_end m new_e,[],None,p)
    | E_Ternary(g,l,r,p) -> E_Ternary(g,l,replace_end r new_e,p)
    | e -> new_e

  let is_cond_modifier = function
    | E_If _ | E_Unless _ | E_Until _ | E_While _ -> true
    | _ -> false

  let well_formed_do guard body = match ends_with guard with
    | E_MethodCall(_,_,Some (E_CodeBlock(false,_,_,_)),_) ->
	raise Dyp.Giveup
    | _ ->()

  let well_formed_return args = match args with
    | [] -> ()
    | hd::tl -> 
	if is_cond_modifier (Utils.last args) then raise Dyp.Giveup;
	match starts_with hd with
	    (* f(x) should be not be f((x))
	       needed e.g. f(x)[y]
	    *)
	  | E_Block _ -> raise Dyp.Giveup
	  | _ -> ()

  let well_formed_command m args = match args with
    | [] -> ()
	(* f(x) should be not be f((x))
	   needed e.g. f(x)[y] *)
    | [E_Block _] -> raise Dyp.Giveup
    | _ -> if List.exists is_cond_modifier args then raise Dyp.Giveup

  let rec hash_literal_as_args args = 
    let rec work acc lst = match lst with
      | [] -> acc
      | (E_Binop(_,Op_ASSOC,_,p))::tl ->
          let rec hash_args acc = function
            | [] -> acc, None
            | [E_Unary(Op_UAmper,_,_) as blk] -> acc, Some blk
            | (E_Binop(_,Op_ASSOC,_,_) as hd)::tl -> 
                hash_args (hd::acc) tl
            | _ -> raise Dyp.Giveup
          in
          let args,blk = hash_args [] lst in
          let acc = E_Hash(false,List.rev args,p)::acc in
          let acc = match blk with
            | None -> acc
            | Some b -> b::acc
          in acc
              
      | hd::tl -> work (hd::acc) tl
    in
      List.rev (work [] args)

  let rec methodcall m args cb pos = 
    let args = hash_literal_as_args args in
    match m,args,cb with
      | _,[E_Empty],_ -> methodcall m [] cb pos

      | E_Return(_), [], None -> m
      | E_Return([],p),args,None -> E_Return(args,p)
      | E_Yield(_), [], None -> m
      | E_Yield([],p),args,None -> E_Yield(args,p)
      | E_Literal(Lit_True,p), [],None
      | E_Literal(Lit_False,p),[],None
      | E_Identifier(_,_,p),     [],None -> m

      | E_Literal _,_,_ -> raise Dyp.Giveup

      | E_Binop(x,Op_SCOPE,y,_),[],None -> m

      | E_Binop(x,Op_DOT,y,p),_,_ -> E_MethodCall(unfold_dot x y p, args, cb,p)
      | _ -> E_MethodCall(m,args,cb,pos)

  and unfold_dot l r pos = 
    match l with
    (* unfold nested a.b.c to become (a.b()).c() *)
      | E_Binop(a,Op_DOT,b,p) ->
	  let l' = methodcall (unfold_dot a b p) [] None p in
	    E_Binop(l',Op_DOT,r,pos)
	      
      | _ -> E_Binop(l,Op_DOT,r,pos)

  and check_for_dot = function
    | E_Binop(l,Op_DOT,r,p) -> methodcall (unfold_dot l r p) [] None p
    | e -> e
	
  and scope l r = 
    let l = check_for_dot l in
      E_Binop(l,Op_SCOPE,r,pos_of l)
	
  let tuple = function
    | [] -> E_Empty
    | [x] -> x
    | lst -> E_Tuple(lst,pos_of (List.hd lst))

  let command_codeblock cmd cb = 
    match cmd with 
      | E_MethodCall(c,args,None,p) -> E_MethodCall(c,args,Some cb,p)
      | E_Binop(_,Op_DOT,_,p)
      | E_Binop(_,Op_SCOPE,_,p) -> E_MethodCall(cmd,[],Some cb,p)
      | E_Identifier(_,_,p) -> E_MethodCall(cmd,[],Some cb,p)
      | _ -> raise Dyp.Giveup

(* sometimes the lexer gets can't properly handle x!= as x(!=) and
   erronously produces (x!)= *)
  let fix_broken_neq l op r = 
    let default = l, op, r in
    match op with
    | Op_ASSIGN -> begin match ends_with l with
	| E_Identifier(k,s,p) ->
	    let len = String.length s in
	      if s.[len-1] == '!'
	      then 
		let s' = String.sub s 0 (len-1) in
		let l' = replace_end l (E_Identifier(k,s',p)) in
		  l', Op_NEQ, r
	      else default
	| _ -> default
      end
    | _ -> default

(* sometimes the lexer gets can't properly handle x=> as x(=>) and
   erronously produces (x=)> *)
  let fix_broken_assoc l op r = 
    let default = l, op, r in
    match op with
    | Op_GT -> begin match ends_with l with
	| E_Identifier(ID_Assign ik,s,p) ->
	    let l' = replace_end l (E_Identifier(ik,s,p)) in
	      l', Op_ASSOC, r
	| E_Literal(Lit_Atom(sc), pos) ->
	    let astr,rest = match List.rev sc with
	      | (Ast.StrChars s)::tl -> s,tl
	      | _ -> "a",[]
	    in
	    let len = String.length astr in
	      if astr.[len-1] == '='
	      then 
		let s' = String.sub astr 0 (len-1) in
		let sc' = List.rev ((Ast.StrChars s')::rest) in
		let l' = replace_end l (E_Literal(Lit_Atom(sc'),pos)) in
		  l', Op_ASSOC, r
	      else default
	| _ -> default
      end
    | _ -> default

  let expr_priority = function
    | E_Unary(Op_UBang,_,_) | E_Unary(Op_UTilde,_,_)| E_Unary(Op_UPlus,_,_) -> 2000
    | E_Unary(Op_UMinus,_,_) -> 1900
    | E_Binop(_,Op_POW,_,_) -> 1800
    | E_Binop(_,Op_DIV,_,_) | E_Binop(_,Op_REM,_,_) | E_Binop(_,Op_TIMES,_,_) -> 1700
    | E_Binop(_,Op_MINUS,_,_) -> 1500
    | E_Binop(_,Op_PLUS,_,_) -> 1500
    | E_Binop(_,Op_LSHIFT,_,_) | E_Binop(_,Op_RSHIFT,_,_) -> 1400
    | E_Binop(_,Op_BAND,_,_) -> 1300
    | E_Binop(_,Op_BOR,_,_) | E_Binop(_,Op_XOR,_,_) -> 1200

    | E_Binop(_,Op_LEQ,_,_) | E_Binop(_,Op_LT,_,_) 
    | E_Binop(_,Op_GEQ,_,_) | E_Binop(_,Op_GT,_,_) -> 1100

    | E_Binop(_,Op_MATCH,_,_) | E_Binop(_,Op_NMATCH,_,_) | E_Binop(_,Op_NEQ,_,_) 
    | E_Binop(_,Op_CMP,_,_) | E_Binop(_,Op_EQ,_,_) | E_Binop(_,Op_EQQ,_,_) -> 1000

    | E_Binop(_,Op_DOT2,_,_) | E_Binop(_,Op_DOT3,_,_) -> 800

    | E_Binop(_,Op_AND,_,_) -> 750
    | E_Binop(_,Op_OR,_,_) -> 700
    | E_Binop(_,Op_ASSIGN,_,_) | E_Binop(_,Op_OP_ASGN _,_,_) -> 600

    | E_Ternary _ -> 500
    | E_Binop(_,Op_ASSOC,_,_) -> 400

    | E_Unary(Op_UNot,_,_) -> 200
    | E_Binop(_,Op_kAND,_,_) | E_Binop(_,Op_kOR,_,_) -> 100

    | E_Binop _ | E_Unary _ | _ -> max_int
	
  let binop_priority = function
    | E_Unary _ -> max_int
    | e -> expr_priority e

  let prune_uop uop arg pos = 
    let e = E_Unary(uop,arg,pos) in
    let p = expr_priority e in
    let p' = expr_priority arg in
      if p' < p then raise Dyp.Giveup
      else e

  let prune_right_assoc l op r = 
    let l,op,r = fix_broken_neq l op r in
    let l,op,r = fix_broken_assoc l op r in
    let e = E_Binop(l,op,r,(pos_of l)) in
    let p = binop_priority e in
    let pl = binop_priority l in
    let pr = binop_priority r in
      if pr < p || pl <= p
      then raise Dyp.Giveup
      else e

  (* right: (x - y) - z 
     prune: x - (y - z)
  *)
  let prune_left_assoc l op r = 
    let l,op,r = fix_broken_neq l op r in
    let l,op,r = fix_broken_assoc l op r in
    let e = E_Binop(l,op,r,(pos_of l)) in
      match l,op,r with
        | _, _, E_Binop(_,Op_ASSIGN,_,_) ->  e

        | _ ->
            let p = binop_priority e in
            let pl = binop_priority l in
            let pr = binop_priority r in
              if pr <= p || pl < p
              then raise Dyp.Giveup
              else e

  let prune_tern e1 e2 e3 pos = 
    let e = E_Ternary(e1,e2,e3,pos) in
    let p = expr_priority e in
    let p1 = expr_priority e1 in      
      (*Printf.eprintf "tern: %s\n" (Ast_printer.string_of_expr e);*)
      if p1 <= p then raise Dyp.Giveup
      else e

  let uniq_list cmp lst =
    let rec u = function
      | [] -> []
      | [x] -> [x]
      | x1::x2::tl ->
	  if cmp x1 x2 = 0
	  then u (x1::tl) else x1 :: (u (x2::tl))
    in
      u (List.sort cmp lst)

  let gup_empty = function
    | E_Empty -> raise Dyp.Giveup
    | _ -> ()

  let do_fail s l to_s =
    let len = List.length l in
      if len > 1 then begin
	Printf.eprintf "<%s>: %d\n" s len;
	List.iter (fun x -> Printf.eprintf " %s\n" (to_s x)) l;
	failwith s
      end

  let rec rhs_do_codeblock = function
    | E_MethodCall(_,_,Some (E_CodeBlock(false,_,_,_)),_) -> true
    | E_Binop(_,_,r,_)
    | E_MethodCall(r,[],None,_)
    | E_Ternary(_,_,r,_) -> rhs_do_codeblock r
    | E_Hash(false,el,_) -> rhs_do_codeblock (Utils.last el)

    | e -> 
        Printf.eprintf "got: %s\n" (Ast_printer.string_of_expr e);
        false

  let resolve_block_delim with_cb no_cb = match with_cb,no_cb with
    | _, E_MethodCall(_,[],None,_) -> 
        Printf.eprintf "here2??\n";[with_cb;no_cb]
    | E_MethodCall(m1',args1,Some do_block,_),
        E_MethodCall(m2',args_ne,None,_) -> 
	(* look for cmd arg1,...,(argn do block end) *)
        if rhs_do_codeblock (Utils.last args_ne)
        then [with_cb]
        else [with_cb;no_cb]
    | _ -> assert false
        
  let merge_binop l newest = 
    let l' = uniq_list Ast.compare_expr l in
    let fail () = 
      let l' = uniq_list Ast.compare_expr (newest::l') in
	do_fail "binop" l' Ast_printer.string_of_expr;
	l'
    in
    let rec nested_assign = function
      | E_Binop(_,(Op_ASSIGN|Op_OP_ASGN _),_,_) -> true
      | E_Binop(_,_,(E_Binop _ as r),_) -> nested_assign r
      | _ -> false
    in
      match l',newest with
        | [E_Binop(_,Op_ASSIGN,_,_)], E_Binop(_,Op_ASSIGN,_,_) ->
            Printf.eprintf "fail1\n";
            fail ()

        | [E_Binop(l,_,_,_)], correct when nested_assign l -> [correct]
        | [correct], E_Binop(l,_,_,_) when nested_assign l -> [correct]

        | _ -> Printf.eprintf "fail2\n";fail()

  let merge_topcall l newest = 
    let l' = uniq_list Ast.compare_expr l in
      match l',newest with
	| [(E_MethodCall(_,_,Some (E_CodeBlock(false,_,_,_)),_) as with_cb)],
	  (E_MethodCall(_,_,None,_) as no_cb)
	| [(E_MethodCall(_,_,None,_) as no_cb)],
	  (E_MethodCall(_,_,Some (E_CodeBlock(false,_,_,_)),_) as with_cb) ->
	    (* resolve "x y{z}" vs "x y do z end" *)
	    resolve_block_delim with_cb no_cb;
	| _ ->
	    let l' = uniq_list Ast.compare_expr (newest::l') in
	      do_fail "topcall" l' Ast_printer.string_of_expr;
	      l'

  let merge_stmt l newest = 
    let l' = uniq_list Ast.compare_expr l in
      match l',newest with
	| [(E_MethodCall(_,_,Some (E_CodeBlock(false,_,_,_)),_) as with_cb)],
	  (E_MethodCall(_,_,None,_) as no_cb)
	| [(E_MethodCall(_,_,None,_) as no_cb)],
	  (E_MethodCall(_,_,Some (E_CodeBlock(false,_,_,_)),_) as with_cb) ->
	    (* resolve "x y{z}" vs "x y do z end" *)
	    resolve_block_delim with_cb no_cb;

	| [E_ExnBlock({body_exprs = [E_Binop(_,Op_ASSIGN,_,_)]},_)],
	    (E_Binop(_,Op_ASSIGN,(E_ExnBlock _),_) as correct)
	| ([E_Binop(_,Op_ASSIGN,(E_ExnBlock _),_) as correct]),
	    E_ExnBlock({body_exprs = [E_Binop(_,Op_ASSIGN,_,_)]},_) ->
	      (* x = y rescue 3 is a special case where the rescue binds
		 solely to "y" and not the full assignment *)
	    [correct]

	| [E_ExnBlock({body_exprs = [E_Binop(_,Op_OP_ASGN _,_,_)]},_) as correct],
	      E_Binop(_,Op_OP_ASGN _,(E_ExnBlock _),_)
	| [E_Binop(_,Op_OP_ASGN _,(E_ExnBlock _),_)],
	      (E_ExnBlock({body_exprs = [E_Binop(_,Op_OP_ASGN _,_,_)]},_) as correct) ->
	      (* However, using any other assign-operator, reverts to the 
                 other semantics *)
	    [correct]

	(* top-level assignment has a higher priority than any other op *)
	| [E_Binop(l,(Op_ASSIGN|Op_OP_ASGN _ as op),r,pos)], (E_Binop _ | E_Ternary _) ->
	    let l,op,r = fix_broken_neq l op r in
	      [E_Binop(l,op,r,pos)]

	(* we can't use is_cond_modifier to check for a rescue modifier,
	   so we do it here *)	   
	| [E_If(E_ExnBlock _,_,_,_) | E_Unless(E_ExnBlock _,_,_,_)
	  | E_Until(_,E_ExnBlock _,_,_) | E_While(_,E_ExnBlock _,_,_)],
	    (E_ExnBlock _ as correct)
	| [(E_ExnBlock _ as correct)], 
	    (E_If(E_ExnBlock _,_,_,_) | E_Unless(E_ExnBlock _,_,_,_)
	    | E_Until(_,E_ExnBlock _,_,_) | E_While(_,E_ExnBlock _,_,_)) ->
	    [correct]

	| _ ->
	    let l' = uniq_list Ast.compare_expr (newest::l') in
	      do_fail "stmt" l' Ast_printer.string_of_expr;
	      l'

  let merge_expr s l newest =
    let l' = uniq_list Ast.compare_expr (newest::l) in
      do_fail s l' Ast_printer.string_of_expr;
      l'

  let merge_expr_list s l newest = 
    let l' = uniq_list Ast.compare_ast (newest::l) in
      do_fail s l' Ast_printer.string_of_ast;
      l'

  let merge_formal_list s l newest = 
    let f x = Utils.format_to_string Ast_printer.format_formals x in
    let l' = uniq_list compare (newest::l) in
      do_fail s l' f;
      l'

  let merge_rest s l n = 
    let l' = n::l in
      do_fail s l' (fun x -> "??");
      l'

  let merge_rescue s l n = 
    let cmp (x1,y1) (x2,y2) = 
      Utils.cmp2 (Ast.compare_expr x1 x2) Ast.compare_expr y1 y2
    in
    let l' = uniq_list cmp (n::l) in
      do_fail s l' 
	(fun (x,y) -> 
	   Printf.sprintf "%s: %s" 
	     (Ast_printer.string_of_expr x)
	     (Ast_printer.string_of_expr y)
	);
      l'

  (*let () = Dyp.dypgen_verbose := max_int*)

  let dyp_merge_eols = merge_rest "eols"
  let dyp_merge_some_eols = merge_rest "some_eols "
  let dyp_merge_main= merge_rest "main"
  let dyp_merge_stmt_list= merge_expr_list "stmt_list"
  let dyp_merge_stmt= merge_stmt (*merge_expr "stmt"*)
  let dyp_merge_topcall= merge_topcall
  let dyp_merge_arg_comma_list_trail= merge_rest "arg_comma_list_trail"
  let dyp_merge_arg_comma_nonempty_list= merge_rest "arg_comma_nonempty_list"
  let dyp_merge_arg_comma_star_list= merge_rest "arg_comma_star_list"
  let dyp_merge_func= merge_expr "func"
  let dyp_merge_star_amper= merge_rest "star_amper"
  let dyp_merge_call_args= merge_expr_list "call_args"
  let dyp_merge_command= merge_expr "command"
  let dyp_merge_command_name= merge_expr "command_name"
  let dyp_merge_binop= merge_binop
  let dyp_merge_arg= merge_binop (*arg "arg"*)
  let dyp_merge_expr= merge_expr "expr"
  let dyp_merge_primary= merge_expr "primary"
  let dyp_merge_array_item = merge_rest "array_item"
  let dyp_merge_array_body_rest= merge_rest "array_body_rest"
  let dyp_merge_array_body= merge_expr_list "array_body"
  let dyp_merge_scoped_id= merge_rest "scoped_id"
  let dyp_merge_class_inheritance= merge_rest "class_inheritance"
  let dyp_merge_do_sep= merge_rest "do_sep"
  let dyp_merge_code_block= merge_rest "code_block"
  let dyp_merge_code_block_body= merge_rest "code_block_body"
  let dyp_merge_formal_arg= merge_rest "formal_arg"
  let dyp_merge_formal_arg_nonempty_list= merge_formal_list "formal_arg_nonempty_list"
  let dyp_merge_formal_arg_list= merge_formal_list "formal_arg_list"
  let dyp_merge_method_formals= merge_formal_list "method_formals"
  let dyp_merge_lhs= merge_rest "lhs"
  let dyp_merge_mlhs= merge_rest "mlhs"
  let dyp_merge_mlhs_clean= merge_expr_list "mlhs_clean"
  let dyp_merge_mlhs_rest= merge_rest "mlhs_rest"
  let dyp_merge_mlhs_item= merge_rest "mlhs_item"
  let dyp_merge_command_codeblock= merge_rest "command_codeblock"
  let dyp_merge_mrhs= merge_expr_list "mrhs"
  let dyp_merge_then_sep= merge_rest "then_sep"
  let dyp_merge_when_clauses= merge_rest "when_clauses"
  let dyp_merge_body_exn= merge_rest "body_exn"
  let dyp_merge_rescue_clause= merge_rescue "rescue_clause"
  let dyp_merge_rescue_list= merge_rest "rescue_list"
  let dyp_merge_case_else= merge_rest "case_else"
  let dyp_merge_ensure_clause= merge_rest "ensure_clause"
  let dyp_merge_if_else_clauses= merge_rest "if_else_clauses"
  let dyp_merge_meth_or_atom= merge_rest "meth_or_atom"
  let dyp_merge_method_name= merge_expr "method_name"
  let dyp_merge_assignable= merge_rest "assignable"
  let dyp_merge_message_identifier= merge_rest "message_identifier"
  let dyp_merge_bin_op= merge_rest "bin_op"
  let dyp_merge_unary_op= merge_rest "unary_op"
  let dyp_merge_keyword_as_id= merge_rest "keyword_as_id"
  let dyp_merge_identifier= merge_rest "identifier"
  let dyp_merge_constant= merge_rest "constant"

  let dyp_merge l n = 
    Printf.eprintf "<all> branches: %d\n%!" (List.length l);
    n::l
    (*Dyp.keep_all*)

  (* DAVIDAN: This function is for parsing type annotation *)
  let parse_type_info t_info pos = 
    let lexbuf = Lexing.from_string t_info in 
    let () = lexbuf.Lexing.lex_curr_p <- pos in
      try TypeAnnotParser.input TypeAnnotLexer.token lexbuf
      with Parsing.Parse_error | Failure("lexing: empty token") ->
        Log.fatal Log.empty
          "Parsing error in type annotation in %s prior to line %d" 
          lexbuf.Lexing.lex_curr_p.Lexing.pos_fname 
          lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum

  let build_annot t annot_str pos = 
    match parse_type_info annot_str pos with
      | None -> t
      | Some annot -> 
          Ast.verify_annotation_name t annot pos;
          E_Annotate(t,annot,pos)

# 1868               "newParser.ml"
let __dypgen_ra_list =
[
((Dyp_symbols.eols,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_eols 
# 763 "newParser.dyp"
(
    ():'dypgen__Obj_eols)
# 1875               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.eols,[Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_EOL],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 1881               "newParser.ml"
 as _1))); _2] -> Obj_eols 
# 764 "newParser.dyp"
(
               ():'dypgen__Obj_eols)
# 1886               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.some_eols,[Dyp.Ter Dyp_symbols.t_T_EOL;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1;`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 1892               "newParser.ml"
 as _2)))] -> Obj_some_eols 
# 767 "newParser.dyp"
(
               ():'dypgen__Obj_some_eols)
# 1897               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.main,[Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_EOF],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 1903               "newParser.ml"
 as _1)));`Real_obj (Obj_stmt_list ( (
# 771 "newParser.dyp"
                  (program:'dypgen__Obj_stmt_list)
# 1907               "newParser.ml"
 as _2))); _3] -> Obj_main 
# 770 "newParser.dyp"
(
                                  ( program ):Ast.expr list)
# 1912               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt_list,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_stmt_list 
# 773 "newParser.dyp"
(
    ([]):'dypgen__Obj_stmt_list)
# 1919               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt_list,[Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt ( (
# 775 "newParser.dyp"
        (s:'dypgen__Obj_stmt)
# 1925               "newParser.ml"
 as _1)))] -> Obj_stmt_list 
# 774 "newParser.dyp"
(
            ( [s] ):'dypgen__Obj_stmt_list)
# 1930               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt_list,[Dyp.Ter Dyp_symbols.t_T_SEMICOLON;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_SEMICOLON  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 1936               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 1940               "newParser.ml"
 as _2)));`Real_obj (Obj_stmt_list ( (
# 776 "newParser.dyp"
                              (ss:'dypgen__Obj_stmt_list)
# 1944               "newParser.ml"
 as _3)))] -> Obj_stmt_list 
# 776 "newParser.dyp"
(
      ( dyp.Dyp.will_shift <- false; ss ):'dypgen__Obj_stmt_list)
# 1949               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt_list,[Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eol_or_semi,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt ( (
# 778 "newParser.dyp"
        (s:'dypgen__Obj_stmt)
# 1955               "newParser.ml"
 as _1)));`Real_obj (Obj_eol_or_semi ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eol_or_semi)
# 1959               "newParser.ml"
 as _2)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 1963               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt_list ( (
# 778 "newParser.dyp"
                                      (ss:'dypgen__Obj_stmt_list)
# 1967               "newParser.ml"
 as _4)))] -> Obj_stmt_list 
# 778 "newParser.dyp"
(
      ( dyp.Dyp.will_shift <- false; s:: ss ):'dypgen__Obj_stmt_list)
# 1972               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.assign_op,[Dyp.Ter Dyp_symbols.t_T_ASSIGN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_ASSIGN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 1978               "newParser.ml"
 as _1))] -> Obj_assign_op 
# 781 "newParser.dyp"
(
             (Op_ASSIGN):'dypgen__Obj_assign_op)
# 1983               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.assign_op,[Dyp.Ter Dyp_symbols.t_T_OP_ASGN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_OP_ASGN  (
# 783 "newParser.dyp"
             (op,pos:string*Lexing.position)
# 1989               "newParser.ml"
 as _1))] -> Obj_assign_op 
# 782 "newParser.dyp"
(
                      ( Op_OP_ASGN (binary_op_of_string op) ):'dypgen__Obj_assign_op)
# 1994               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scope_begin,[Dyp.Ter Dyp_symbols.t_K_BEGIN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_BEGIN  (
# 786 "newParser.dyp"
                     (pos:Lexing.position)
# 2000               "newParser.ml"
 as _1))] -> Obj_scope_begin 
# 785 "newParser.dyp"
(
                           (enter_scope dyp;pos):'dypgen__Obj_scope_begin)
# 2005               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scope_end,[Dyp.Ter Dyp_symbols.t_K_END],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_END  (
# 787 "newParser.dyp"
                   (pos:Lexing.position)
# 2011               "newParser.ml"
 as _1))] -> Obj_scope_end 
# 786 "newParser.dyp"
(
                           (enter_scope dyp;pos):'dypgen__Obj_scope_end)
# 2016               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scope_def,[Dyp.Ter Dyp_symbols.t_K_DEF],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_DEF  (
# 788 "newParser.dyp"
                   (pos:string*Lexing.position)
# 2022               "newParser.ml"
 as _1))] -> Obj_scope_def 
# 787 "newParser.dyp"
(
                           (enter_scope dyp;pos):'dypgen__Obj_scope_def)
# 2027               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scope_class,[Dyp.Ter Dyp_symbols.t_K_CLASS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_CLASS  (
# 789 "newParser.dyp"
                     (t_info,pos:string*Lexing.position)
# 2033               "newParser.ml"
 as _1))] -> Obj_scope_class 
# 788 "newParser.dyp"
(
                                   (enter_scope dyp;t_info,pos):'dypgen__Obj_scope_class)
# 2038               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scope_module,[Dyp.Ter Dyp_symbols.t_K_MODULE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_MODULE  (
# 790 "newParser.dyp"
                      (t_info,pos:string*Lexing.position)
# 2044               "newParser.ml"
 as _1))] -> Obj_scope_module 
# 789 "newParser.dyp"
(
                                   (enter_scope dyp;t_info,pos):'dypgen__Obj_scope_module)
# 2049               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.seen_id,[Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_identifier ( (
# 791 "newParser.dyp"
                   (i:'dypgen__Obj_identifier)
# 2055               "newParser.ml"
 as _1)))] -> Obj_seen_id 
# 790 "newParser.dyp"
(
                       (seen dyp i;i):'dypgen__Obj_seen_id)
# 2060               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Ter Dyp_symbols.t_T_CAST],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_CAST  (
# 794 "newParser.dyp"
          (annot,e,pos:string * string * Lexing.position)
# 2066               "newParser.ml"
 as _1))] -> Obj_stmt 
# 794 "newParser.dyp"
(
      ( build_annot (E_Identifier(ID_Lowercase,e,pos)) annot pos):'dypgen__Obj_stmt)
# 2071               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Ter Dyp_symbols.t_K_ALIAS;Dyp.Non_ter (Dyp_symbols.meth_or_atom,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.meth_or_atom,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ALIAS  (
# 797 "newParser.dyp"
           (pos:Lexing.position)
# 2077               "newParser.ml"
 as _1));`Real_obj (Obj_meth_or_atom ( (
# 797 "newParser.dyp"
                             (e1:'dypgen__Obj_meth_or_atom)
# 2081               "newParser.ml"
 as _2)));`Real_obj (Obj_meth_or_atom ( (
# 797 "newParser.dyp"
                                              (e2:'dypgen__Obj_meth_or_atom)
# 2085               "newParser.ml"
 as _3)))] -> Obj_stmt 
# 796 "newParser.dyp"
(
                                                   ( E_Alias(e1,e2,pos) ):'dypgen__Obj_stmt)
# 2090               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Ter Dyp_symbols.t_K_UNDEF;Dyp.Non_ter (Dyp_symbols.meth_or_atom_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_UNDEF  (
# 798 "newParser.dyp"
           (pos:Lexing.position)
# 2096               "newParser.ml"
 as _1));`Real_obj (Obj_meth_or_atom_list ( (
# 798 "newParser.dyp"
                                  (e1:'dypgen__Obj_meth_or_atom_list)
# 2100               "newParser.ml"
 as _2)))] -> Obj_stmt 
# 797 "newParser.dyp"
(
                                       ( E_Undef(e1,pos) ):'dypgen__Obj_stmt)
# 2105               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_IF;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt ( (
# 799 "newParser.dyp"
        (s:'dypgen__Obj_stmt)
# 2111               "newParser.ml"
 as _1)));`Real_obj (Obj_K_IF  (
# 799 "newParser.dyp"
                (pos:Lexing.position)
# 2115               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2119               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt ( (
# 799 "newParser.dyp"
                                   (guard:'dypgen__Obj_stmt)
# 2123               "newParser.ml"
 as _4)))] -> Obj_stmt 
# 799 "newParser.dyp"
(
      ( if is_cond_modifier guard then raise Dyp.Giveup; E_If(guard, [s], [],pos) ):'dypgen__Obj_stmt)
# 2128               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_UNLESS;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt ( (
# 801 "newParser.dyp"
        (s:'dypgen__Obj_stmt)
# 2134               "newParser.ml"
 as _1)));`Real_obj (Obj_K_UNLESS  (
# 801 "newParser.dyp"
                    (pos:Lexing.position)
# 2138               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2142               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt ( (
# 801 "newParser.dyp"
                                   (guard:'dypgen__Obj_stmt)
# 2146               "newParser.ml"
 as _4)))] -> Obj_stmt 
# 801 "newParser.dyp"
(
      ( if is_cond_modifier guard then raise Dyp.Giveup; E_Unless(guard, [s], [],pos) ):'dypgen__Obj_stmt)
# 2151               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_UNTIL;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt ( (
# 803 "newParser.dyp"
        (s:'dypgen__Obj_stmt)
# 2157               "newParser.ml"
 as _1)));`Real_obj (Obj_K_UNTIL  (
# 803 "newParser.dyp"
                   (pos:Lexing.position)
# 2161               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2165               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt ( (
# 803 "newParser.dyp"
                                   (guard:'dypgen__Obj_stmt)
# 2169               "newParser.ml"
 as _4)))] -> Obj_stmt 
# 803 "newParser.dyp"
(
      ( if is_cond_modifier guard then raise Dyp.Giveup; 
	E_Until(is_exnblock s,guard, [s],pos) ):'dypgen__Obj_stmt)
# 2175               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_WHILE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt ( (
# 806 "newParser.dyp"
        (s:'dypgen__Obj_stmt)
# 2181               "newParser.ml"
 as _1)));`Real_obj (Obj_K_WHILE  (
# 806 "newParser.dyp"
                   (pos:Lexing.position)
# 2185               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2189               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt ( (
# 806 "newParser.dyp"
                                   (guard:'dypgen__Obj_stmt)
# 2193               "newParser.ml"
 as _4)))] -> Obj_stmt 
# 806 "newParser.dyp"
(
      ( if is_cond_modifier guard then raise Dyp.Giveup;
	E_While(is_exnblock s,guard, [s],pos) ):'dypgen__Obj_stmt)
# 2199               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_RESCUE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt ( (
# 809 "newParser.dyp"
        (s:'dypgen__Obj_stmt)
# 2205               "newParser.ml"
 as _1)));`Real_obj (Obj_K_RESCUE  (
# 809 "newParser.dyp"
                    (pos:Lexing.position)
# 2209               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2213               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt ( (
# 809 "newParser.dyp"
                                   (guard:'dypgen__Obj_stmt)
# 2217               "newParser.ml"
 as _4)))] -> Obj_stmt 
# 809 "newParser.dyp"
(
      ( if is_cond_modifier guard then raise Dyp.Giveup;
	E_ExnBlock({body_exprs = [s]; rescue_exprs = [(E_Empty,guard)];
		    ensure_expr = []; else_expr = []}, pos)):'dypgen__Obj_stmt)
# 2224               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.lhs_assign_op,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_RESCUE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_lhs_assign_op ( (
# 817 "newParser.dyp"
                 (l,op:'dypgen__Obj_lhs_assign_op)
# 2230               "newParser.ml"
 as _1)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2234               "newParser.ml"
 as _2)));`Real_obj (Obj_arg ( (
# 817 "newParser.dyp"
                                (r:'dypgen__Obj_arg)
# 2238               "newParser.ml"
 as _3)));`Real_obj (Obj_K_RESCUE  (
# 817 "newParser.dyp"
                                            (pos:Lexing.position)
# 2242               "newParser.ml"
 as _4));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2246               "newParser.ml"
 as _5)));`Real_obj (Obj_stmt ( (
# 817 "newParser.dyp"
                                                           (guard:'dypgen__Obj_stmt)
# 2250               "newParser.ml"
 as _6)))] -> Obj_stmt 
# 817 "newParser.dyp"
(
      ( if is_cond_modifier guard then raise Dyp.Giveup;
	let r' = E_ExnBlock({body_exprs = [r]; rescue_exprs = [(E_Empty,guard)];
			     ensure_expr = []; else_expr = []}, pos)
	in (*prune_binop l op r'*) E_Binop(l,op,r',pos)):'dypgen__Obj_stmt)
# 2258               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.scope_begin,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_LBRACE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scope_begin ( (
# 823 "newParser.dyp"
               (pos:'dypgen__Obj_scope_begin)
# 2264               "newParser.ml"
 as _1)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2268               "newParser.ml"
 as _2)));`Real_obj (Obj_T_LBRACE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2272               "newParser.ml"
 as _3));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2276               "newParser.ml"
 as _4)));`Real_obj (Obj_stmt_list ( (
# 823 "newParser.dyp"
                                                 (body:'dypgen__Obj_stmt_list)
# 2280               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RBRACE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2284               "newParser.ml"
 as _6))] -> Obj_stmt 
# 823 "newParser.dyp"
(
      (leave_scope dyp; E_BeginBlock(body,pos)):'dypgen__Obj_stmt)
# 2289               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.scope_end,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_LBRACE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scope_end ( (
# 825 "newParser.dyp"
             (pos:'dypgen__Obj_scope_end)
# 2295               "newParser.ml"
 as _1)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2299               "newParser.ml"
 as _2)));`Real_obj (Obj_T_LBRACE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2303               "newParser.ml"
 as _3));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2307               "newParser.ml"
 as _4)));`Real_obj (Obj_stmt_list ( (
# 825 "newParser.dyp"
                                               (body:'dypgen__Obj_stmt_list)
# 2311               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RBRACE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2315               "newParser.ml"
 as _6))] -> Obj_stmt 
# 825 "newParser.dyp"
(
      (leave_scope dyp; E_EndBlock(body,pos)):'dypgen__Obj_stmt)
# 2320               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.topcall,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_topcall ( (
# 828 "newParser.dyp"
           (c:'dypgen__Obj_topcall)
# 2326               "newParser.ml"
 as _1)))] -> Obj_stmt 
# 827 "newParser.dyp"
(
               ( c ):'dypgen__Obj_stmt)
# 2331               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.mlhs_assign_op,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.mrhs,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_mlhs_assign_op ( (
# 829 "newParser.dyp"
                  (l,op:'dypgen__Obj_mlhs_assign_op)
# 2337               "newParser.ml"
 as _1)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2341               "newParser.ml"
 as _2)));`Real_obj (Obj_mrhs ( (
# 829 "newParser.dyp"
                                  (r:'dypgen__Obj_mrhs)
# 2345               "newParser.ml"
 as _3)))] -> Obj_stmt 
# 829 "newParser.dyp"
(
      ( (*prune_binop (tuple l) op (tuple r)*) 
	let lhs = tuple l in
	  E_Binop(lhs,op,(tuple r),(pos_of lhs))
      ):'dypgen__Obj_stmt)
# 2353               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 834 "newParser.dyp"
        (e:'dypgen__Obj_expr)
# 2359               "newParser.ml"
 as _1)))] -> Obj_stmt 
# 833 "newParser.dyp"
(
            ( e ):'dypgen__Obj_stmt)
# 2364               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.stmt,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 835 "newParser.dyp"
           (pos:Lexing.position)
# 2370               "newParser.ml"
 as _1));`Real_obj (Obj_primary ( (
# 835 "newParser.dyp"
                        (e:'dypgen__Obj_primary)
# 2374               "newParser.ml"
 as _2)))] -> Obj_stmt 
# 834 "newParser.dyp"
(
                            ( E_Unary(Op_UStar,e,pos) ):'dypgen__Obj_stmt)
# 2379               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lhs_assign_op,[Dyp.Non_ter (Dyp_symbols.lhs,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.assign_op,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_lhs ( (
# 837 "newParser.dyp"
                  (l:'dypgen__Obj_lhs)
# 2385               "newParser.ml"
 as _1)));`Real_obj (Obj_assign_op ( (
# 837 "newParser.dyp"
                               (op:'dypgen__Obj_assign_op)
# 2389               "newParser.ml"
 as _2)))] -> Obj_lhs_assign_op 
# 836 "newParser.dyp"
(
                                    (seen dyp l; l,op):'dypgen__Obj_lhs_assign_op)
# 2394               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_assign_op,[Dyp.Non_ter (Dyp_symbols.mlhs,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.assign_op,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_mlhs ( (
# 838 "newParser.dyp"
                    (l:'dypgen__Obj_mlhs)
# 2400               "newParser.ml"
 as _1)));`Real_obj (Obj_assign_op ( (
# 838 "newParser.dyp"
                                 (op:'dypgen__Obj_assign_op)
# 2404               "newParser.ml"
 as _2)))] -> Obj_mlhs_assign_op 
# 837 "newParser.dyp"
(
                                      (List.iter (seen dyp) l; l,op):'dypgen__Obj_mlhs_assign_op)
# 2409               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.topcall,[Dyp.Non_ter (Dyp_symbols.func,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_func ( (
# 841 "newParser.dyp"
        (f:'dypgen__Obj_func)
# 2415               "newParser.ml"
 as _1)))] -> Obj_topcall 
# 840 "newParser.dyp"
(
            ( f ):'dypgen__Obj_topcall)
# 2420               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.topcall,[Dyp.Non_ter (Dyp_symbols.func,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.code_block,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_func ( (
# 842 "newParser.dyp"
        (c:'dypgen__Obj_func)
# 2426               "newParser.ml"
 as _1)));`Real_obj (Obj_code_block ( (
# 842 "newParser.dyp"
                      (cb:'dypgen__Obj_code_block)
# 2430               "newParser.ml"
 as _2)))] -> Obj_topcall 
# 842 "newParser.dyp"
(
      ( match c with
	| E_MethodCall(m,args,None,pos) -> methodcall m args (Some cb) pos
	| _ -> methodcall c [] (Some cb) (pos_of c)):'dypgen__Obj_topcall)
# 2437               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.topcall,[Dyp.Non_ter (Dyp_symbols.command,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command ( (
# 847 "newParser.dyp"
           (c:'dypgen__Obj_command)
# 2443               "newParser.ml"
 as _1)))] -> Obj_topcall 
# 846 "newParser.dyp"
(
               (c):'dypgen__Obj_topcall)
# 2448               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.topcall,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.do_codeblock,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 848 "newParser.dyp"
                (m:'dypgen__Obj_command_name)
# 2454               "newParser.ml"
 as _1)));`Real_obj (Obj_call_args ( (
# 848 "newParser.dyp"
                             (args:'dypgen__Obj_call_args)
# 2458               "newParser.ml"
 as _2)));`Real_obj (Obj_do_codeblock ( (
# 848 "newParser.dyp"
                                                (cb:'dypgen__Obj_do_codeblock)
# 2462               "newParser.ml"
 as _3)))] -> Obj_topcall 
# 848 "newParser.dyp"
(
      ( well_formed_command m args;
	methodcall m args (Some cb) (pos_of m)):'dypgen__Obj_topcall)
# 2468               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg_comma_list_trail,[Dyp.Non_ter (Dyp_symbols.arg_comma_star_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg_comma_star_list ( (
# 853 "newParser.dyp"
                       (e:'dypgen__Obj_arg_comma_star_list)
# 2474               "newParser.ml"
 as _1)))] -> Obj_arg_comma_list_trail 
# 852 "newParser.dyp"
(
                           (e):'dypgen__Obj_arg_comma_list_trail)
# 2479               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg_comma_list_trail,[Dyp.Non_ter (Dyp_symbols.arg_comma_star_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg_comma_star_list ( (
# 854 "newParser.dyp"
                       (e:'dypgen__Obj_arg_comma_star_list)
# 2485               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2489               "newParser.ml"
 as _2))] -> Obj_arg_comma_list_trail 
# 853 "newParser.dyp"
(
                                   (e):'dypgen__Obj_arg_comma_list_trail)
# 2494               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg_comma_nonempty_list,[Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg ( (
# 857 "newParser.dyp"
       (a:'dypgen__Obj_arg)
# 2500               "newParser.ml"
 as _1)))] -> Obj_arg_comma_nonempty_list 
# 856 "newParser.dyp"
(
           ( [a] ):'dypgen__Obj_arg_comma_nonempty_list)
# 2505               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg_comma_nonempty_list,[Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg_comma_nonempty_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg ( (
# 858 "newParser.dyp"
       (a:'dypgen__Obj_arg)
# 2511               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2515               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2519               "newParser.ml"
 as _3)));`Real_obj (Obj_arg_comma_nonempty_list ( (
# 858 "newParser.dyp"
                                               (al:'dypgen__Obj_arg_comma_nonempty_list)
# 2523               "newParser.ml"
 as _4)))] -> Obj_arg_comma_nonempty_list 
# 858 "newParser.dyp"
(
      ( dyp.Dyp.will_shift <- false; a::al ):'dypgen__Obj_arg_comma_nonempty_list)
# 2528               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg_comma_star_list,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_arg_comma_star_list 
# 861 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_arg_comma_star_list)
# 2535               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg_comma_star_list,[Dyp.Non_ter (Dyp_symbols.star_amper,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_star_amper ( (
# 863 "newParser.dyp"
              (a:'dypgen__Obj_star_amper)
# 2541               "newParser.ml"
 as _1)))] -> Obj_arg_comma_star_list 
# 862 "newParser.dyp"
(
                  ( a ):'dypgen__Obj_arg_comma_star_list)
# 2546               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg_comma_star_list,[Dyp.Non_ter (Dyp_symbols.arg_comma_nonempty_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg_comma_nonempty_list ( (
# 864 "newParser.dyp"
                           (args:'dypgen__Obj_arg_comma_nonempty_list)
# 2552               "newParser.ml"
 as _1)))] -> Obj_arg_comma_star_list 
# 863 "newParser.dyp"
(
                                  (args):'dypgen__Obj_arg_comma_star_list)
# 2557               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg_comma_star_list,[Dyp.Non_ter (Dyp_symbols.arg_comma_nonempty_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.star_amper,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg_comma_nonempty_list ( (
# 865 "newParser.dyp"
                           (args:'dypgen__Obj_arg_comma_nonempty_list)
# 2563               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2567               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2571               "newParser.ml"
 as _3)));`Real_obj (Obj_star_amper ( (
# 865 "newParser.dyp"
                                                         (a:'dypgen__Obj_star_amper)
# 2575               "newParser.ml"
 as _4)))] -> Obj_arg_comma_star_list 
# 865 "newParser.dyp"
(
      ( dyp.Dyp.will_shift <- false;
	args @ a ):'dypgen__Obj_arg_comma_star_list)
# 2581               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.func,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.lparen,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 870 "newParser.dyp"
                (c:'dypgen__Obj_command_name)
# 2587               "newParser.ml"
 as _1)));`Real_obj (Obj_lparen ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_lparen)
# 2591               "newParser.ml"
 as _2)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2595               "newParser.ml"
 as _3)));`Real_obj (Obj_call_args ( (
# 870 "newParser.dyp"
                                         (args:'dypgen__Obj_call_args)
# 2599               "newParser.ml"
 as _4)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2603               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2607               "newParser.ml"
 as _6))] -> Obj_func 
# 870 "newParser.dyp"
(
      (
	match args with
	  | [] -> E_MethodCall(c,args,None,pos_of c) 
	  | _ -> methodcall c args None (pos_of c)
      ):'dypgen__Obj_func)
# 2616               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.func,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.lparen,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.command,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 876 "newParser.dyp"
                (c:'dypgen__Obj_command_name)
# 2622               "newParser.ml"
 as _1)));`Real_obj (Obj_lparen ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_lparen)
# 2626               "newParser.ml"
 as _2)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2630               "newParser.ml"
 as _3)));`Real_obj (Obj_command ( (
# 876 "newParser.dyp"
                                       (a:'dypgen__Obj_command)
# 2634               "newParser.ml"
 as _4)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2638               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2642               "newParser.ml"
 as _6))] -> Obj_func 
# 876 "newParser.dyp"
(
      (E_MethodCall(c,[a],None,pos_of c)):'dypgen__Obj_func)
# 2647               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.star_amper,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 880 "newParser.dyp"
           (pos:Lexing.position)
# 2653               "newParser.ml"
 as _1));`Real_obj (Obj_arg ( (
# 880 "newParser.dyp"
                    (a:'dypgen__Obj_arg)
# 2657               "newParser.ml"
 as _2)))] -> Obj_star_amper 
# 879 "newParser.dyp"
(
                        ( [E_Unary(Op_UStar,a,pos)] ):'dypgen__Obj_star_amper)
# 2662               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.star_amper,[Dyp.Ter Dyp_symbols.t_T_UAMPER;Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_UAMPER  (
# 881 "newParser.dyp"
            (pos:Lexing.position)
# 2668               "newParser.ml"
 as _1));`Real_obj (Obj_arg ( (
# 881 "newParser.dyp"
                     (a:'dypgen__Obj_arg)
# 2672               "newParser.ml"
 as _2)))] -> Obj_star_amper 
# 880 "newParser.dyp"
(
                         ( [E_Unary(Op_UAmper,a,pos)] ):'dypgen__Obj_star_amper)
# 2677               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.star_amper,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_UAMPER;Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 882 "newParser.dyp"
           (pos1:Lexing.position)
# 2683               "newParser.ml"
 as _1));`Real_obj (Obj_arg ( (
# 882 "newParser.dyp"
                     (a1:'dypgen__Obj_arg)
# 2687               "newParser.ml"
 as _2)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2691               "newParser.ml"
 as _3));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2695               "newParser.ml"
 as _4)));`Real_obj (Obj_T_UAMPER  (
# 882 "newParser.dyp"
                                               (pos2:Lexing.position)
# 2699               "newParser.ml"
 as _5));`Real_obj (Obj_arg ( (
# 882 "newParser.dyp"
                                                         (a2:'dypgen__Obj_arg)
# 2703               "newParser.ml"
 as _6)))] -> Obj_star_amper 
# 882 "newParser.dyp"
(
      ( [E_Unary(Op_UStar,a1,pos1); E_Unary(Op_UAmper,a2,pos2)] ):'dypgen__Obj_star_amper)
# 2708               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.call_args,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_call_args 
# 885 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_call_args)
# 2715               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.call_args,[Dyp.Non_ter (Dyp_symbols.arg_comma_star_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg_comma_star_list ( (
# 887 "newParser.dyp"
                       (args:'dypgen__Obj_arg_comma_star_list)
# 2721               "newParser.ml"
 as _1)))] -> Obj_call_args 
# 886 "newParser.dyp"
(
                              ( args ):'dypgen__Obj_call_args)
# 2726               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.call_args,[Dyp.Ter Dyp_symbols.t_T_LBRACK;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LBRACK  (
# 888 "newParser.dyp"
            (pos:Lexing.position)
# 2732               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2736               "newParser.ml"
 as _2)));`Real_obj (Obj_call_args ( (
# 888 "newParser.dyp"
                                (args:'dypgen__Obj_call_args)
# 2740               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2744               "newParser.ml"
 as _4)));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2748               "newParser.ml"
 as _5))] -> Obj_call_args 
# 887 "newParser.dyp"
(
                                                     ( [E_Array(args,pos)] ):'dypgen__Obj_call_args)
# 2753               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command,[Dyp.Ter Dyp_symbols.t_K_YIELD;Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_YIELD  (
# 891 "newParser.dyp"
           (pos:Lexing.position)
# 2759               "newParser.ml"
 as _1));`Real_obj (Obj_call_args ( (
# 891 "newParser.dyp"
                          (args:'dypgen__Obj_call_args)
# 2763               "newParser.ml"
 as _2)))] -> Obj_command 
# 891 "newParser.dyp"
(
      ( well_formed_return args;
	E_Yield(args,pos) ):'dypgen__Obj_command)
# 2769               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 894 "newParser.dyp"
                (m:'dypgen__Obj_command_name)
# 2775               "newParser.ml"
 as _1)));`Real_obj (Obj_call_args ( (
# 894 "newParser.dyp"
                             (args:'dypgen__Obj_call_args)
# 2779               "newParser.ml"
 as _2)))] -> Obj_command 
# 894 "newParser.dyp"
(
      ( well_formed_command m args;
	methodcall m args None (pos_of m)):'dypgen__Obj_command)
# 2785               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.command,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 897 "newParser.dyp"
                (cmd:'dypgen__Obj_command_name)
# 2791               "newParser.ml"
 as _1)));`Real_obj (Obj_command ( (
# 897 "newParser.dyp"
                             (cmd2:'dypgen__Obj_command)
# 2795               "newParser.ml"
 as _2)))] -> Obj_command 
# 897 "newParser.dyp"
(
    (well_formed_command cmd [cmd2];
     methodcall cmd [cmd2] None (pos_of cmd)):'dypgen__Obj_command)
# 2801               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command_name,[Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_identifier ( (
# 902 "newParser.dyp"
              (id:'dypgen__Obj_identifier)
# 2807               "newParser.ml"
 as _1)))] -> Obj_command_name 
# 901 "newParser.dyp"
(
                   (id):'dypgen__Obj_command_name)
# 2812               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command_name,[Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_DOT;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.message_identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_primary ( (
# 903 "newParser.dyp"
           (p:'dypgen__Obj_primary)
# 2818               "newParser.ml"
 as _1)));`Real_obj (Obj_T_DOT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2822               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2826               "newParser.ml"
 as _3)));`Real_obj (Obj_message_identifier ( (
# 903 "newParser.dyp"
                                            (m:'dypgen__Obj_message_identifier)
# 2830               "newParser.ml"
 as _4)))] -> Obj_command_name 
# 903 "newParser.dyp"
(
      ( unfold_dot p m (pos_of p)):'dypgen__Obj_command_name)
# 2835               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command_name,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_DOT;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.message_identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 905 "newParser.dyp"
                (p:'dypgen__Obj_command_name)
# 2841               "newParser.ml"
 as _1)));`Real_obj (Obj_T_DOT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2845               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2849               "newParser.ml"
 as _3)));`Real_obj (Obj_message_identifier ( (
# 905 "newParser.dyp"
                                                 (m:'dypgen__Obj_message_identifier)
# 2853               "newParser.ml"
 as _4)))] -> Obj_command_name 
# 905 "newParser.dyp"
(
      ( unfold_dot p m (pos_of p)):'dypgen__Obj_command_name)
# 2858               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command_name,[Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_SCOPE;Dyp.Non_ter (Dyp_symbols.message_identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_primary ( (
# 907 "newParser.dyp"
           (p:'dypgen__Obj_primary)
# 2864               "newParser.ml"
 as _1)));`Real_obj (Obj_T_SCOPE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2868               "newParser.ml"
 as _2));`Real_obj (Obj_message_identifier ( (
# 907 "newParser.dyp"
                                         (m:'dypgen__Obj_message_identifier)
# 2872               "newParser.ml"
 as _3)))] -> Obj_command_name 
# 907 "newParser.dyp"
(
      ( scope p m ):'dypgen__Obj_command_name)
# 2877               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command_name,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_SCOPE;Dyp.Non_ter (Dyp_symbols.message_identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 909 "newParser.dyp"
                (p:'dypgen__Obj_command_name)
# 2883               "newParser.ml"
 as _1)));`Real_obj (Obj_T_SCOPE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 2887               "newParser.ml"
 as _2));`Real_obj (Obj_message_identifier ( (
# 909 "newParser.dyp"
                                              (m:'dypgen__Obj_message_identifier)
# 2891               "newParser.ml"
 as _3)))] -> Obj_command_name 
# 909 "newParser.dyp"
(
      ( scope p m ):'dypgen__Obj_command_name)
# 2896               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lhs_prune_binop,[Dyp.Non_ter (Dyp_symbols.lhs,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_lhs ( (
# 912 "newParser.dyp"
                    (l:'dypgen__Obj_lhs)
# 2902               "newParser.ml"
 as _1)))] -> Obj_lhs_prune_binop 
# 911 "newParser.dyp"
(
                        (match l with E_Binop _ -> raise Dyp.Giveup | _ -> l):'dypgen__Obj_lhs_prune_binop)
# 2907               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lhs_pruned_assign_op,[Dyp.Non_ter (Dyp_symbols.lhs_prune_binop,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.assign_op,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_lhs_prune_binop ( (
# 913 "newParser.dyp"
                                     (l:'dypgen__Obj_lhs_prune_binop)
# 2913               "newParser.ml"
 as _1)));`Real_obj (Obj_assign_op ( (
# 913 "newParser.dyp"
                                                  (op:'dypgen__Obj_assign_op)
# 2917               "newParser.ml"
 as _2)))] -> Obj_lhs_pruned_assign_op 
# 912 "newParser.dyp"
(
                                                       (seen dyp l; l,op):'dypgen__Obj_lhs_pruned_assign_op)
# 2922               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg,[Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_primary ( (
# 916 "newParser.dyp"
           (p:'dypgen__Obj_primary)
# 2928               "newParser.ml"
 as _1)))] -> Obj_arg 
# 915 "newParser.dyp"
(
               (p):'dypgen__Obj_arg)
# 2933               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg,[Dyp.Non_ter (Dyp_symbols.unary_op,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_unary_op ( (
# 917 "newParser.dyp"
            (o,pos:'dypgen__Obj_unary_op)
# 2939               "newParser.ml"
 as _1)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2943               "newParser.ml"
 as _2)));`Real_obj (Obj_arg ( (
# 917 "newParser.dyp"
                            (a:'dypgen__Obj_arg)
# 2947               "newParser.ml"
 as _3)))] -> Obj_arg 
# 916 "newParser.dyp"
(
                                ( prune_uop o a pos ):'dypgen__Obj_arg)
# 2952               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg,[Dyp.Non_ter (Dyp_symbols.lhs_pruned_assign_op,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_lhs_pruned_assign_op ( (
# 918 "newParser.dyp"
                        (l,op:'dypgen__Obj_lhs_pruned_assign_op)
# 2958               "newParser.ml"
 as _1)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2962               "newParser.ml"
 as _2)));`Real_obj (Obj_arg ( (
# 918 "newParser.dyp"
                                       (r:'dypgen__Obj_arg)
# 2966               "newParser.ml"
 as _3)))] -> Obj_arg 
# 918 "newParser.dyp"
(
      ( prune_right_assoc l op r ):'dypgen__Obj_arg)
# 2971               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg,[Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.bin_op,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg ( (
# 921 "newParser.dyp"
       (l:'dypgen__Obj_arg)
# 2977               "newParser.ml"
 as _1)));`Real_obj (Obj_bin_op ( (
# 921 "newParser.dyp"
                 (bop:'dypgen__Obj_bin_op)
# 2981               "newParser.ml"
 as _2)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 2985               "newParser.ml"
 as _3)));`Real_obj (Obj_arg ( (
# 921 "newParser.dyp"
                               (r:'dypgen__Obj_arg)
# 2989               "newParser.ml"
 as _4)))] -> Obj_arg 
# 920 "newParser.dyp"
(
                                  ( prune_left_assoc l bop r ):'dypgen__Obj_arg)
# 2994               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg,[Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_ANDOP;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg ( (
# 922 "newParser.dyp"
       (l:'dypgen__Obj_arg)
# 3000               "newParser.ml"
 as _1)));`Real_obj (Obj_T_ANDOP  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3004               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3008               "newParser.ml"
 as _3)));`Real_obj (Obj_arg ( (
# 922 "newParser.dyp"
                           (r:'dypgen__Obj_arg)
# 3012               "newParser.ml"
 as _4)))] -> Obj_arg 
# 921 "newParser.dyp"
(
                               ( prune_left_assoc l Op_AND r ):'dypgen__Obj_arg)
# 3017               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg,[Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_OROP;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg ( (
# 923 "newParser.dyp"
       (l:'dypgen__Obj_arg)
# 3023               "newParser.ml"
 as _1)));`Real_obj (Obj_T_OROP  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3027               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3031               "newParser.ml"
 as _3)));`Real_obj (Obj_arg ( (
# 923 "newParser.dyp"
                          (r:'dypgen__Obj_arg)
# 3035               "newParser.ml"
 as _4)))] -> Obj_arg 
# 922 "newParser.dyp"
(
                               ( prune_left_assoc l Op_OR r ):'dypgen__Obj_arg)
# 3040               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.arg,[Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_QUESTION;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COLON;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg ( (
# 924 "newParser.dyp"
       (e1:'dypgen__Obj_arg)
# 3046               "newParser.ml"
 as _1)));`Real_obj (Obj_T_QUESTION  (
# 924 "newParser.dyp"
                      (pos:Lexing.position)
# 3050               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3054               "newParser.ml"
 as _3)));`Real_obj (Obj_expr ( (
# 924 "newParser.dyp"
                                     (e2:'dypgen__Obj_expr)
# 3058               "newParser.ml"
 as _4)));`Real_obj (Obj_T_COLON  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3062               "newParser.ml"
 as _5));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3066               "newParser.ml"
 as _6)));`Real_obj (Obj_expr ( (
# 924 "newParser.dyp"
                                                           (e3:'dypgen__Obj_expr)
# 3070               "newParser.ml"
 as _7)))] -> Obj_arg 
# 924 "newParser.dyp"
(
    ( prune_tern e1 e2 e3 (pos_of e1) ):'dypgen__Obj_arg)
# 3075               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_K_RETURN;Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RETURN  (
# 928 "newParser.dyp"
            (pos:Lexing.position)
# 3081               "newParser.ml"
 as _1));`Real_obj (Obj_call_args ( (
# 928 "newParser.dyp"
                           (args:'dypgen__Obj_call_args)
# 3085               "newParser.ml"
 as _2)))] -> Obj_expr 
# 928 "newParser.dyp"
(
      ( well_formed_return args; E_Return(args,pos) ):'dypgen__Obj_expr)
# 3090               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_K_RETURN;Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RETURN  (
# 930 "newParser.dyp"
            (pos:Lexing.position)
# 3096               "newParser.ml"
 as _1));`Real_obj (Obj_arg ( (
# 930 "newParser.dyp"
                     (p:'dypgen__Obj_arg)
# 3100               "newParser.ml"
 as _2)))] -> Obj_expr 
# 930 "newParser.dyp"
(
      ( match p with
	| E_Block([x],_) -> E_Return([x],pos)
	| arg -> if is_cond_modifier arg then raise Dyp.Giveup;
	    E_Return([arg],pos) ):'dypgen__Obj_expr)
# 3108               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_K_YIELD;Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_YIELD  (
# 936 "newParser.dyp"
           (pos:Lexing.position)
# 3114               "newParser.ml"
 as _1));`Real_obj (Obj_call_args ( (
# 936 "newParser.dyp"
                          (args:'dypgen__Obj_call_args)
# 3118               "newParser.ml"
 as _2)))] -> Obj_expr 
# 936 "newParser.dyp"
(
      ( well_formed_return args;
	E_Yield(args,pos) ):'dypgen__Obj_expr)
# 3124               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_AND;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 939 "newParser.dyp"
        (e1:'dypgen__Obj_expr)
# 3130               "newParser.ml"
 as _1)));`Real_obj (Obj_K_AND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3134               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3138               "newParser.ml"
 as _3)));`Real_obj (Obj_expr ( (
# 939 "newParser.dyp"
                            (e2:'dypgen__Obj_expr)
# 3142               "newParser.ml"
 as _4)))] -> Obj_expr 
# 938 "newParser.dyp"
(
                                 ( prune_left_assoc e1 Op_kAND e2 ):'dypgen__Obj_expr)
# 3147               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_OR;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 940 "newParser.dyp"
        (e1:'dypgen__Obj_expr)
# 3153               "newParser.ml"
 as _1)));`Real_obj (Obj_K_OR  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3157               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3161               "newParser.ml"
 as _3)));`Real_obj (Obj_expr ( (
# 940 "newParser.dyp"
                           (e2:'dypgen__Obj_expr)
# 3165               "newParser.ml"
 as _4)))] -> Obj_expr 
# 939 "newParser.dyp"
(
                                 ( prune_left_assoc e1 Op_kOR e2 ):'dypgen__Obj_expr)
# 3170               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_K_NOT;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_NOT  (
# 941 "newParser.dyp"
         (pos:Lexing.position)
# 3176               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3180               "newParser.ml"
 as _2)));`Real_obj (Obj_expr ( (
# 941 "newParser.dyp"
                        (e:'dypgen__Obj_expr)
# 3184               "newParser.ml"
 as _3)))] -> Obj_expr 
# 940 "newParser.dyp"
(
                                      ( prune_uop Op_UNot e pos):'dypgen__Obj_expr)
# 3189               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_T_BANG;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_BANG  (
# 942 "newParser.dyp"
          (pos:Lexing.position)
# 3195               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3199               "newParser.ml"
 as _2)));`Real_obj (Obj_expr ( (
# 942 "newParser.dyp"
                         (e:'dypgen__Obj_expr)
# 3203               "newParser.ml"
 as _3)))] -> Obj_expr 
# 941 "newParser.dyp"
(
                                      ( prune_uop Op_UBang e pos):'dypgen__Obj_expr)
# 3208               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.command,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command ( (
# 943 "newParser.dyp"
           (c:'dypgen__Obj_command)
# 3214               "newParser.ml"
 as _1)))] -> Obj_expr 
# 942 "newParser.dyp"
(
                            (c):'dypgen__Obj_expr)
# 3219               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg ( (
# 944 "newParser.dyp"
       (a:'dypgen__Obj_arg)
# 3225               "newParser.ml"
 as _1)))] -> Obj_expr 
# 943 "newParser.dyp"
(
                            ( a ):'dypgen__Obj_expr)
# 3230               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_T_LPAREN;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LPAREN  (
# 947 "newParser.dyp"
            (pos:Lexing.position)
# 3236               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3240               "newParser.ml"
 as _2)));`Real_obj (Obj_stmt_list ( (
# 947 "newParser.dyp"
                                (ss:'dypgen__Obj_stmt_list)
# 3244               "newParser.ml"
 as _3)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3248               "newParser.ml"
 as _4))] -> Obj_primary 
# 947 "newParser.dyp"
(
      ( (* don't collapse the block here to prevent the disambiguation
	  rules from erroneously firing	*) E_Block(ss,pos) ):'dypgen__Obj_primary)
# 3254               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.constant,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_constant ( (
# 950 "newParser.dyp"
            (c:'dypgen__Obj_constant)
# 3260               "newParser.ml"
 as _1)))] -> Obj_primary 
# 949 "newParser.dyp"
(
                ( c ):'dypgen__Obj_primary)
# 3265               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_identifier ( (
# 951 "newParser.dyp"
              (id:'dypgen__Obj_identifier)
# 3271               "newParser.ml"
 as _1)))] -> Obj_primary 
# 950 "newParser.dyp"
(
                   ( id ):'dypgen__Obj_primary)
# 3276               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 952 "newParser.dyp"
                (c:'dypgen__Obj_command_name)
# 3282               "newParser.ml"
 as _1)))] -> Obj_primary 
# 951 "newParser.dyp"
(
                    ( methodcall c [] None (pos_of c)):'dypgen__Obj_primary)
# 3287               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.code_block,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 953 "newParser.dyp"
                (c:'dypgen__Obj_command_name)
# 3293               "newParser.ml"
 as _1)));`Real_obj (Obj_code_block ( (
# 953 "newParser.dyp"
                              (cb:'dypgen__Obj_code_block)
# 3297               "newParser.ml"
 as _2)))] -> Obj_primary 
# 952 "newParser.dyp"
(
                                   ( command_codeblock c cb ):'dypgen__Obj_primary)
# 3302               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_SCOPE;Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_primary ( (
# 954 "newParser.dyp"
           (p:'dypgen__Obj_primary)
# 3308               "newParser.ml"
 as _1)));`Real_obj (Obj_T_SCOPE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3312               "newParser.ml"
 as _2));`Real_obj (Obj_identifier ( (
# 954 "newParser.dyp"
                                 (id:'dypgen__Obj_identifier)
# 3316               "newParser.ml"
 as _3)))] -> Obj_primary 
# 953 "newParser.dyp"
(
                                      ( scope p id ):'dypgen__Obj_primary)
# 3321               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_T_USCOPE;Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USCOPE  (
# 955 "newParser.dyp"
            (pos:Lexing.position)
# 3327               "newParser.ml"
 as _1));`Real_obj (Obj_identifier ( (
# 955 "newParser.dyp"
                            (id:'dypgen__Obj_identifier)
# 3331               "newParser.ml"
 as _2)))] -> Obj_primary 
# 954 "newParser.dyp"
(
                                 ( E_Unary(Op_UScope,id,pos) ):'dypgen__Obj_primary)
# 3336               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_LBRACK_ARG;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg_comma_list_trail,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_primary ( (
# 956 "newParser.dyp"
           (p:'dypgen__Obj_primary)
# 3342               "newParser.ml"
 as _1)));`Real_obj (Obj_T_LBRACK_ARG  (
# 956 "newParser.dyp"
                           (pos1:Lexing.position)
# 3346               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3350               "newParser.ml"
 as _3)));`Real_obj (Obj_arg_comma_list_trail ( (
# 956 "newParser.dyp"
                                                           (args:'dypgen__Obj_arg_comma_list_trail)
# 3354               "newParser.ml"
 as _4)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3358               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3362               "newParser.ml"
 as _6))] -> Obj_primary 
# 956 "newParser.dyp"
(
      ( methodcall (E_Binop(p,Op_DOT,E_Operator(Op_AREF,pos1),pos_of p)) args None (pos_of p)):'dypgen__Obj_primary)
# 3367               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_T_LBRACK;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.array_body,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LBRACK  (
# 959 "newParser.dyp"
            (pos:Lexing.position)
# 3373               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3377               "newParser.ml"
 as _2)));`Real_obj (Obj_array_body ( (
# 959 "newParser.dyp"
                                 (body:'dypgen__Obj_array_body)
# 3381               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3385               "newParser.ml"
 as _4)));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3389               "newParser.ml"
 as _5))] -> Obj_primary 
# 958 "newParser.dyp"
(
                                                      ( E_Array(body,pos) ):'dypgen__Obj_primary)
# 3394               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_T_LBRACE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.array_body,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LBRACE  (
# 960 "newParser.dyp"
            (pos:Lexing.position)
# 3400               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3404               "newParser.ml"
 as _2)));`Real_obj (Obj_array_body ( (
# 960 "newParser.dyp"
                                 (body:'dypgen__Obj_array_body)
# 3408               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3412               "newParser.ml"
 as _4)));`Real_obj (Obj_T_RBRACE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3416               "newParser.ml"
 as _5))] -> Obj_primary 
# 959 "newParser.dyp"
(
                                                      ( E_Hash(true,body,pos) ):'dypgen__Obj_primary)
# 3421               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_RETURN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RETURN  (
# 961 "newParser.dyp"
            (pos:Lexing.position)
# 3427               "newParser.ml"
 as _1))] -> Obj_primary 
# 960 "newParser.dyp"
(
                  ( E_Return([],pos)):'dypgen__Obj_primary)
# 3432               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_YIELD],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_YIELD  (
# 964 "newParser.dyp"
           (pos:Lexing.position)
# 3438               "newParser.ml"
 as _1))] -> Obj_primary 
# 963 "newParser.dyp"
(
                 ( E_Yield([],pos)):'dypgen__Obj_primary)
# 3443               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_YIELD;Dyp.Non_ter (Dyp_symbols.lparen,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_YIELD  (
# 965 "newParser.dyp"
           (pos:Lexing.position)
# 3449               "newParser.ml"
 as _1));`Real_obj (Obj_lparen ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_lparen)
# 3453               "newParser.ml"
 as _2)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3457               "newParser.ml"
 as _3)));`Real_obj (Obj_call_args ( (
# 965 "newParser.dyp"
                                      (args:'dypgen__Obj_call_args)
# 3461               "newParser.ml"
 as _4)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3465               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3469               "newParser.ml"
 as _6))] -> Obj_primary 
# 964 "newParser.dyp"
(
                                                           ( E_Yield(args,pos)):'dypgen__Obj_primary)
# 3474               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_YIELD;Dyp.Non_ter (Dyp_symbols.lparen,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_YIELD  (
# 966 "newParser.dyp"
           (pos:Lexing.position)
# 3480               "newParser.ml"
 as _1));`Real_obj (Obj_lparen ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_lparen)
# 3484               "newParser.ml"
 as _2)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3488               "newParser.ml"
 as _3)));`Real_obj (Obj_expr ( (
# 966 "newParser.dyp"
                                 (arg:'dypgen__Obj_expr)
# 3492               "newParser.ml"
 as _4)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3496               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3500               "newParser.ml"
 as _6))] -> Obj_primary 
# 965 "newParser.dyp"
(
                                                     ( E_Yield([arg],pos)):'dypgen__Obj_primary)
# 3505               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.func,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_func ( (
# 968 "newParser.dyp"
        (f:'dypgen__Obj_func)
# 3511               "newParser.ml"
 as _1)))] -> Obj_primary 
# 967 "newParser.dyp"
(
            ( f ):'dypgen__Obj_primary)
# 3516               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.func,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.code_block,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_func ( (
# 969 "newParser.dyp"
        (f:'dypgen__Obj_func)
# 3522               "newParser.ml"
 as _1)));`Real_obj (Obj_code_block ( (
# 969 "newParser.dyp"
                      (cb:'dypgen__Obj_code_block)
# 3526               "newParser.ml"
 as _2)))] -> Obj_primary 
# 968 "newParser.dyp"
(
                           (match f with
      | E_MethodCall(m,args,None,pos) -> methodcall m args (Some cb) pos
      | _ -> methodcall f [] (Some cb) (pos_of f)):'dypgen__Obj_primary)
# 3533               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_IF;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.then_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.if_else_clauses,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_IF  (
# 973 "newParser.dyp"
        (pos:Lexing.position)
# 3539               "newParser.ml"
 as _1));`Real_obj (Obj_expr ( (
# 973 "newParser.dyp"
                  (guard:'dypgen__Obj_expr)
# 3543               "newParser.ml"
 as _2)));`Real_obj (Obj_then_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_then_sep)
# 3547               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt_list ( (
# 973 "newParser.dyp"
                                            (body:'dypgen__Obj_stmt_list)
# 3551               "newParser.ml"
 as _4)));`Real_obj (Obj_if_else_clauses ( (
# 973 "newParser.dyp"
                                                                  (else_e:'dypgen__Obj_if_else_clauses)
# 3555               "newParser.ml"
 as _5)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3559               "newParser.ml"
 as _6))] -> Obj_primary 
# 973 "newParser.dyp"
(
    ( E_If(guard,body,else_e,pos) ):'dypgen__Obj_primary)
# 3564               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_UNLESS;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.then_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.case_else,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_UNLESS  (
# 976 "newParser.dyp"
            (pos:Lexing.position)
# 3570               "newParser.ml"
 as _1));`Real_obj (Obj_expr ( (
# 976 "newParser.dyp"
                      (guard:'dypgen__Obj_expr)
# 3574               "newParser.ml"
 as _2)));`Real_obj (Obj_then_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_then_sep)
# 3578               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt_list ( (
# 976 "newParser.dyp"
                                                (body:'dypgen__Obj_stmt_list)
# 3582               "newParser.ml"
 as _4)));`Real_obj (Obj_case_else ( (
# 976 "newParser.dyp"
                                                                (else_e:'dypgen__Obj_case_else)
# 3586               "newParser.ml"
 as _5)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3590               "newParser.ml"
 as _6))] -> Obj_primary 
# 976 "newParser.dyp"
(
    ( E_Unless(guard,body, else_e,pos) ):'dypgen__Obj_primary)
# 3595               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_WHILE;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.do_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_WHILE  (
# 979 "newParser.dyp"
           (pos:Lexing.position)
# 3601               "newParser.ml"
 as _1));`Real_obj (Obj_expr ( (
# 979 "newParser.dyp"
                     (guard:'dypgen__Obj_expr)
# 3605               "newParser.ml"
 as _2)));`Real_obj (Obj_do_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_do_sep)
# 3609               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt_list ( (
# 979 "newParser.dyp"
                                             (body:'dypgen__Obj_stmt_list)
# 3613               "newParser.ml"
 as _4)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3617               "newParser.ml"
 as _5))] -> Obj_primary 
# 979 "newParser.dyp"
(
    ( well_formed_do guard body; E_While(false,guard,body,pos) ):'dypgen__Obj_primary)
# 3622               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_UNTIL;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.do_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_UNTIL  (
# 982 "newParser.dyp"
           (pos:Lexing.position)
# 3628               "newParser.ml"
 as _1));`Real_obj (Obj_expr ( (
# 982 "newParser.dyp"
                     (guard:'dypgen__Obj_expr)
# 3632               "newParser.ml"
 as _2)));`Real_obj (Obj_do_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_do_sep)
# 3636               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt_list ( (
# 982 "newParser.dyp"
                                             (body:'dypgen__Obj_stmt_list)
# 3640               "newParser.ml"
 as _4)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3644               "newParser.ml"
 as _5))] -> Obj_primary 
# 982 "newParser.dyp"
(
    ( well_formed_do guard body;E_Until(false,guard,body,pos) ):'dypgen__Obj_primary)
# 3649               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_CASE;Dyp.Non_ter (Dyp_symbols.some_eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.when_clauses,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.case_else,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_CASE  (
# 985 "newParser.dyp"
          (pos:Lexing.position)
# 3655               "newParser.ml"
 as _1));`Real_obj (Obj_some_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_some_eols)
# 3659               "newParser.ml"
 as _2)));`Real_obj (Obj_when_clauses ( (
# 985 "newParser.dyp"
                                      (whens:'dypgen__Obj_when_clauses)
# 3663               "newParser.ml"
 as _3)));`Real_obj (Obj_case_else ( (
# 985 "newParser.dyp"
                                                       (else_e:'dypgen__Obj_case_else)
# 3667               "newParser.ml"
 as _4)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3671               "newParser.ml"
 as _5))] -> Obj_primary 
# 985 "newParser.dyp"
(
    ( E_Case({case_guard = E_Empty; case_whens = whens; case_else = else_e},pos) ):'dypgen__Obj_primary)
# 3676               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_CASE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.when_clauses,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.case_else,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_CASE  (
# 988 "newParser.dyp"
          (pos:Lexing.position)
# 3682               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3686               "newParser.ml"
 as _2)));`Real_obj (Obj_expr ( (
# 988 "newParser.dyp"
                         (e:'dypgen__Obj_expr)
# 3690               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3694               "newParser.ml"
 as _4)));`Real_obj (Obj_when_clauses ( (
# 988 "newParser.dyp"
                                              (whens:'dypgen__Obj_when_clauses)
# 3698               "newParser.ml"
 as _5)));`Real_obj (Obj_case_else ( (
# 988 "newParser.dyp"
                                                               (else_e:'dypgen__Obj_case_else)
# 3702               "newParser.ml"
 as _6)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3706               "newParser.ml"
 as _7))] -> Obj_primary 
# 988 "newParser.dyp"
(
    ( E_Case({case_guard = e; case_whens = whens; case_else = else_e},pos) ):'dypgen__Obj_primary)
# 3711               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_CASE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_SEMICOLON;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.when_clauses,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.case_else,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_CASE  (
# 991 "newParser.dyp"
          (pos:Lexing.position)
# 3717               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3721               "newParser.ml"
 as _2)));`Real_obj (Obj_expr ( (
# 991 "newParser.dyp"
                         (e:'dypgen__Obj_expr)
# 3725               "newParser.ml"
 as _3)));`Real_obj (Obj_T_SEMICOLON  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3729               "newParser.ml"
 as _4));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3733               "newParser.ml"
 as _5)));`Real_obj (Obj_when_clauses ( (
# 991 "newParser.dyp"
                                                          (whens:'dypgen__Obj_when_clauses)
# 3737               "newParser.ml"
 as _6)));`Real_obj (Obj_case_else ( (
# 991 "newParser.dyp"
                                                                           (else_e:'dypgen__Obj_case_else)
# 3741               "newParser.ml"
 as _7)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3745               "newParser.ml"
 as _8))] -> Obj_primary 
# 992 "newParser.dyp"
(
    ( E_Case({case_guard = e; case_whens = whens; case_else = else_e},pos) ):'dypgen__Obj_primary)
# 3750               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_FOR;Dyp.Non_ter (Dyp_symbols.formal_arg_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_IN;Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.do_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_FOR  (
# 995 "newParser.dyp"
         (pos:Lexing.position)
# 3756               "newParser.ml"
 as _1));`Real_obj (Obj_formal_arg_list ( (
# 995 "newParser.dyp"
                              (vars:'dypgen__Obj_formal_arg_list)
# 3760               "newParser.ml"
 as _2)));`Real_obj (Obj_K_IN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3764               "newParser.ml"
 as _3));`Real_obj (Obj_arg ( (
# 995 "newParser.dyp"
                                             (range:'dypgen__Obj_arg)
# 3768               "newParser.ml"
 as _4)));`Real_obj (Obj_do_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_do_sep)
# 3772               "newParser.ml"
 as _5)));`Real_obj (Obj_stmt_list ( (
# 995 "newParser.dyp"
                                                                     (body:'dypgen__Obj_stmt_list)
# 3776               "newParser.ml"
 as _6)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3780               "newParser.ml"
 as _7))] -> Obj_primary 
# 995 "newParser.dyp"
(
    ( well_formed_do range body; E_For(vars,range,body,pos) ):'dypgen__Obj_primary)
# 3785               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Ter Dyp_symbols.t_K_lBEGIN;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.body_exn,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_lBEGIN  (
# 998 "newParser.dyp"
            (pos:Lexing.position)
# 3791               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3795               "newParser.ml"
 as _2)));`Real_obj (Obj_body_exn ( (
# 998 "newParser.dyp"
                               (body:'dypgen__Obj_body_exn)
# 3799               "newParser.ml"
 as _3)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3803               "newParser.ml"
 as _4))] -> Obj_primary 
# 997 "newParser.dyp"
(
                                              ( E_ExnBlock(body,pos) ):'dypgen__Obj_primary)
# 3808               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.scope_class,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.scoped_id,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.class_inheritance,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.do_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.body_exn,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scope_class ( (
# 1000 "newParser.dyp"
               (t_info, pos:'dypgen__Obj_scope_class)
# 3814               "newParser.ml"
 as _1)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3818               "newParser.ml"
 as _2)));`Real_obj (Obj_scoped_id ( (
# 1000 "newParser.dyp"
                                           (class_name:'dypgen__Obj_scoped_id)
# 3822               "newParser.ml"
 as _3)));`Real_obj (Obj_class_inheritance ( (
# 1000 "newParser.dyp"
                                                                         (inh:'dypgen__Obj_class_inheritance)
# 3826               "newParser.ml"
 as _4)));`Real_obj (Obj_do_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_do_sep)
# 3830               "newParser.ml"
 as _5)));`Real_obj (Obj_body_exn ( (
# 1001 "newParser.dyp"
              (body:'dypgen__Obj_body_exn)
# 3834               "newParser.ml"
 as _6)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3838               "newParser.ml"
 as _7))] -> Obj_primary 
# 1001 "newParser.dyp"
(
      ( leave_scope dyp;
        build_annot (E_ClassDef(class_name, inh, body, pos)) t_info pos):'dypgen__Obj_primary)
# 3844               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.scope_class,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_LSHFT;Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.body_exn,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scope_class ( (
# 1005 "newParser.dyp"
               (t_info, pos:'dypgen__Obj_scope_class)
# 3850               "newParser.ml"
 as _1)));`Real_obj (Obj_T_LSHFT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3854               "newParser.ml"
 as _2));`Real_obj (Obj_arg ( (
# 1005 "newParser.dyp"
                                        (id:'dypgen__Obj_arg)
# 3858               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3862               "newParser.ml"
 as _4)));`Real_obj (Obj_body_exn ( (
# 1005 "newParser.dyp"
                                                          (body:'dypgen__Obj_body_exn)
# 3866               "newParser.ml"
 as _5)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3870               "newParser.ml"
 as _6))] -> Obj_primary 
# 1005 "newParser.dyp"
(
      ( leave_scope dyp;
        let t = E_ClassDef(E_Empty, Some (Inst_Inherit id), body, pos) in
          build_annot t t_info pos
      ):'dypgen__Obj_primary)
# 3878               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.scope_module,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.scoped_id,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.body_exn,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scope_module ( (
# 1011 "newParser.dyp"
                (t_info, pos:'dypgen__Obj_scope_module)
# 3884               "newParser.ml"
 as _1)));`Real_obj (Obj_scoped_id ( (
# 1011 "newParser.dyp"
                                       (name:'dypgen__Obj_scoped_id)
# 3888               "newParser.ml"
 as _2)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3892               "newParser.ml"
 as _3)));`Real_obj (Obj_body_exn ( (
# 1011 "newParser.dyp"
                                                           (body:'dypgen__Obj_body_exn)
# 3896               "newParser.ml"
 as _4)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3900               "newParser.ml"
 as _5))] -> Obj_primary 
# 1011 "newParser.dyp"
(
      ( leave_scope dyp;
        build_annot (E_ModuleDef (name,body, pos)) t_info pos
      ):'dypgen__Obj_primary)
# 3907               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.primary,[Dyp.Non_ter (Dyp_symbols.scope_def,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.method_name,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.method_formals,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.body_exn,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scope_def ( (
# 1016 "newParser.dyp"
             (t_info,pos:'dypgen__Obj_scope_def)
# 3913               "newParser.ml"
 as _1)));`Real_obj (Obj_method_name ( (
# 1016 "newParser.dyp"
                                     (meth_name:'dypgen__Obj_method_name)
# 3917               "newParser.ml"
 as _2)));`Real_obj (Obj_method_formals ( (
# 1016 "newParser.dyp"
                                                               (formals:'dypgen__Obj_method_formals)
# 3921               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3925               "newParser.ml"
 as _4)));`Real_obj (Obj_body_exn ( (
# 1017 "newParser.dyp"
              (body:'dypgen__Obj_body_exn)
# 3929               "newParser.ml"
 as _5)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3933               "newParser.ml"
 as _6))] -> Obj_primary 
# 1017 "newParser.dyp"
(
      ( leave_scope dyp;
        let t = E_MethodDef (meth_name, formals, body, pos) in
          build_annot t t_info pos
      ):'dypgen__Obj_primary)
# 3941               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.array_item,[Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg ( (
# 1024 "newParser.dyp"
       (e:'dypgen__Obj_arg)
# 3947               "newParser.ml"
 as _1)))] -> Obj_array_item 
# 1023 "newParser.dyp"
(
           ( e ):'dypgen__Obj_array_item)
# 3952               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.array_item,[Dyp.Non_ter (Dyp_symbols.constant,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_LBRACK;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_constant ( (
# 1025 "newParser.dyp"
            (c:'dypgen__Obj_constant)
# 3958               "newParser.ml"
 as _1)));`Real_obj (Obj_T_LBRACK  (
# 1025 "newParser.dyp"
                        (pos1:Lexing.position)
# 3962               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3966               "newParser.ml"
 as _3)));`Real_obj (Obj_call_args ( (
# 1025 "newParser.dyp"
                                             (args:'dypgen__Obj_call_args)
# 3970               "newParser.ml"
 as _4)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 3974               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 3978               "newParser.ml"
 as _6))] -> Obj_array_item 
# 1025 "newParser.dyp"
(
      (methodcall
	 (E_Binop(c, Op_DOT, E_Operator(Op_AREF,pos1), pos_of c)) 
	 args None (pos_of c)
      ):'dypgen__Obj_array_item)
# 3986               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.array_body_rest,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_array_body_rest 
# 1031 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_array_body_rest)
# 3993               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.array_body_rest,[Dyp.Non_ter (Dyp_symbols.array_item,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_array_item ( (
# 1033 "newParser.dyp"
              (e:'dypgen__Obj_array_item)
# 3999               "newParser.ml"
 as _1)))] -> Obj_array_body_rest 
# 1032 "newParser.dyp"
(
                  ( [e] ):'dypgen__Obj_array_body_rest)
# 4004               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.array_body_rest,[Dyp.Non_ter (Dyp_symbols.star_amper,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_star_amper ( (
# 1034 "newParser.dyp"
              (es:'dypgen__Obj_star_amper)
# 4010               "newParser.ml"
 as _1)))] -> Obj_array_body_rest 
# 1033 "newParser.dyp"
(
                   ( es ):'dypgen__Obj_array_body_rest)
# 4015               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.array_body_rest,[Dyp.Non_ter (Dyp_symbols.array_item,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.array_body_rest,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_array_item ( (
# 1035 "newParser.dyp"
              (e:'dypgen__Obj_array_item)
# 4021               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4025               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4029               "newParser.ml"
 as _3)));`Real_obj (Obj_array_body_rest ( (
# 1035 "newParser.dyp"
                                              (es:'dypgen__Obj_array_body_rest)
# 4033               "newParser.ml"
 as _4)))] -> Obj_array_body_rest 
# 1035 "newParser.dyp"
(
      ( dyp.Dyp.will_shift <- false;
	e::es ):'dypgen__Obj_array_body_rest)
# 4039               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.array_body,[Dyp.Non_ter (Dyp_symbols.command,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command ( (
# 1040 "newParser.dyp"
           (c:'dypgen__Obj_command)
# 4045               "newParser.ml"
 as _1)))] -> Obj_array_body 
# 1039 "newParser.dyp"
(
               ( [c] ):'dypgen__Obj_array_body)
# 4050               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.array_body,[Dyp.Non_ter (Dyp_symbols.array_body_rest,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_array_body_rest ( (
# 1041 "newParser.dyp"
                   (es:'dypgen__Obj_array_body_rest)
# 4056               "newParser.ml"
 as _1)))] -> Obj_array_body 
# 1040 "newParser.dyp"
(
                        ( es ):'dypgen__Obj_array_body)
# 4061               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scoped_id,[Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_identifier ( (
# 1044 "newParser.dyp"
              (id:'dypgen__Obj_identifier)
# 4067               "newParser.ml"
 as _1)))] -> Obj_scoped_id 
# 1043 "newParser.dyp"
(
                   ( id ):'dypgen__Obj_scoped_id)
# 4072               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scoped_id,[Dyp.Ter Dyp_symbols.t_T_USCOPE;Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USCOPE  (
# 1045 "newParser.dyp"
            (pos:Lexing.position)
# 4078               "newParser.ml"
 as _1));`Real_obj (Obj_identifier ( (
# 1045 "newParser.dyp"
                            (id:'dypgen__Obj_identifier)
# 4082               "newParser.ml"
 as _2)))] -> Obj_scoped_id 
# 1044 "newParser.dyp"
(
                                 ( E_Unary(Op_UScope,id,pos)):'dypgen__Obj_scoped_id)
# 4087               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scoped_id,[Dyp.Non_ter (Dyp_symbols.scoped_id,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_SCOPE;Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scoped_id ( (
# 1046 "newParser.dyp"
             (id1:'dypgen__Obj_scoped_id)
# 4093               "newParser.ml"
 as _1)));`Real_obj (Obj_T_SCOPE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4097               "newParser.ml"
 as _2));`Real_obj (Obj_identifier ( (
# 1046 "newParser.dyp"
                                     (id2:'dypgen__Obj_identifier)
# 4101               "newParser.ml"
 as _3)))] -> Obj_scoped_id 
# 1045 "newParser.dyp"
(
                                              ( E_Binop(id1,Op_SCOPE,id2,pos_of id1) ):'dypgen__Obj_scoped_id)
# 4106               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.scoped_id,[Dyp.Non_ter (Dyp_symbols.scoped_id,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_SCOPE;Dyp.Non_ter (Dyp_symbols.keyword_as_id,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scoped_id ( (
# 1047 "newParser.dyp"
             (id1:'dypgen__Obj_scoped_id)
# 4112               "newParser.ml"
 as _1)));`Real_obj (Obj_T_SCOPE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4116               "newParser.ml"
 as _2));`Real_obj (Obj_keyword_as_id ( (
# 1047 "newParser.dyp"
                                        (id2:'dypgen__Obj_keyword_as_id)
# 4120               "newParser.ml"
 as _3)))] -> Obj_scoped_id 
# 1046 "newParser.dyp"
(
                                              ( E_Binop(id1,Op_SCOPE,id2,pos_of id1) ):'dypgen__Obj_scoped_id)
# 4125               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.class_inheritance,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_class_inheritance 
# 1049 "newParser.dyp"
(
   (None):'dypgen__Obj_class_inheritance)
# 4132               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.class_inheritance,[Dyp.Ter Dyp_symbols.t_T_LT;Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4138               "newParser.ml"
 as _1));`Real_obj (Obj_primary ( (
# 1051 "newParser.dyp"
               (p:'dypgen__Obj_primary)
# 4142               "newParser.ml"
 as _2)))] -> Obj_class_inheritance 
# 1050 "newParser.dyp"
(
                   ( Some (Class_Inherit p) ):'dypgen__Obj_class_inheritance)
# 4147               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.do_sep,[Dyp.Ter Dyp_symbols.t_T_SEMICOLON;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_SEMICOLON  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4153               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4157               "newParser.ml"
 as _2)))] -> Obj_do_sep 
# 1053 "newParser.dyp"
(
                       ():'dypgen__Obj_do_sep)
# 4162               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.do_sep,[Dyp.Ter Dyp_symbols.t_T_COLON;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_COLON  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4168               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4172               "newParser.ml"
 as _2)))] -> Obj_do_sep 
# 1054 "newParser.dyp"
(
                   ():'dypgen__Obj_do_sep)
# 4177               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.do_sep,[Dyp.Non_ter (Dyp_symbols.some_eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_some_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_some_eols)
# 4183               "newParser.ml"
 as _1)))] -> Obj_do_sep 
# 1055 "newParser.dyp"
(
                ():'dypgen__Obj_do_sep)
# 4188               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.do_sep,[Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_DO;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4194               "newParser.ml"
 as _1)));`Real_obj (Obj_K_DO  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4198               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4202               "newParser.ml"
 as _3)))] -> Obj_do_sep 
# 1056 "newParser.dyp"
(
                     ():'dypgen__Obj_do_sep)
# 4207               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.brace_codeblock,[Dyp.Ter Dyp_symbols.t_T_LBRACE_ARG;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.code_block_body,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LBRACE_ARG  (
# 1060 "newParser.dyp"
                (pos:Lexing.position)
# 4213               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4217               "newParser.ml"
 as _2)));`Real_obj (Obj_code_block_body ( (
# 1060 "newParser.dyp"
                                          (b:'dypgen__Obj_code_block_body)
# 4221               "newParser.ml"
 as _3)));`Real_obj (Obj_T_RBRACE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4225               "newParser.ml"
 as _4))] -> Obj_brace_codeblock 
# 1060 "newParser.dyp"
(
      ( let args,body = b in E_CodeBlock(true,args,body,pos) ):'dypgen__Obj_brace_codeblock)
# 4230               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.do_codeblock,[Dyp.Ter Dyp_symbols.t_K_DO;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.code_block_body,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_DO  (
# 1064 "newParser.dyp"
        (pos:Lexing.position)
# 4236               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4240               "newParser.ml"
 as _2)));`Real_obj (Obj_code_block_body ( (
# 1064 "newParser.dyp"
                                  (b:'dypgen__Obj_code_block_body)
# 4244               "newParser.ml"
 as _3)));`Real_obj (Obj_K_lEND  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4248               "newParser.ml"
 as _4))] -> Obj_do_codeblock 
# 1064 "newParser.dyp"
(
      ( let args,body = b in E_CodeBlock(false,args,body,pos) ):'dypgen__Obj_do_codeblock)
# 4253               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.code_block,[Dyp.Non_ter (Dyp_symbols.brace_codeblock,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_brace_codeblock ( (
# 1068 "newParser.dyp"
                   (cb:'dypgen__Obj_brace_codeblock)
# 4259               "newParser.ml"
 as _1)))] -> Obj_code_block 
# 1067 "newParser.dyp"
(
                        (cb):'dypgen__Obj_code_block)
# 4264               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.code_block,[Dyp.Non_ter (Dyp_symbols.do_codeblock,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_do_codeblock ( (
# 1069 "newParser.dyp"
                (cb:'dypgen__Obj_do_codeblock)
# 4270               "newParser.ml"
 as _1)))] -> Obj_code_block 
# 1068 "newParser.dyp"
(
                     (cb):'dypgen__Obj_code_block)
# 4275               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.code_block_body,[Dyp.Ter Dyp_symbols.t_T_OROP;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_OROP  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4281               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4285               "newParser.ml"
 as _2)));`Real_obj (Obj_stmt_list ( (
# 1073 "newParser.dyp"
                         (body:'dypgen__Obj_stmt_list)
# 4289               "newParser.ml"
 as _3)))] -> Obj_code_block_body 
# 1072 "newParser.dyp"
(
                                ((Some [],body)):'dypgen__Obj_code_block_body)
# 4294               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.code_block_body,[Dyp.Ter Dyp_symbols.t_T_VBAR;Dyp.Non_ter (Dyp_symbols.formal_arg_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_VBAR;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_VBAR  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4300               "newParser.ml"
 as _1));`Real_obj (Obj_formal_arg_list ( (
# 1074 "newParser.dyp"
                          (args:'dypgen__Obj_formal_arg_list)
# 4304               "newParser.ml"
 as _2)));`Real_obj (Obj_T_VBAR  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4308               "newParser.ml"
 as _3));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4312               "newParser.ml"
 as _4)));`Real_obj (Obj_stmt_list ( (
# 1074 "newParser.dyp"
                                                      (body:'dypgen__Obj_stmt_list)
# 4316               "newParser.ml"
 as _5)))] -> Obj_code_block_body 
# 1074 "newParser.dyp"
(
      ((Some args,body)):'dypgen__Obj_code_block_body)
# 4321               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.code_block_body,[Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt_list ( (
# 1076 "newParser.dyp"
             (body:'dypgen__Obj_stmt_list)
# 4327               "newParser.ml"
 as _1)))] -> Obj_code_block_body 
# 1076 "newParser.dyp"
(
    ((None, body)):'dypgen__Obj_code_block_body)
# 4332               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg,[Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_identifier ( (
# 1080 "newParser.dyp"
              (id:'dypgen__Obj_identifier)
# 4338               "newParser.ml"
 as _1)))] -> Obj_formal_arg 
# 1079 "newParser.dyp"
(
                   ( seen dyp id; Formal_id id ):'dypgen__Obj_formal_arg)
# 4343               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg,[Dyp.Ter Dyp_symbols.t_T_UAMPER;Dyp.Ter Dyp_symbols.t_T_LID],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_UAMPER  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4349               "newParser.ml"
 as _1));`Real_obj (Obj_T_LID  (
# 1081 "newParser.dyp"
                  (id,pos:string * Lexing.position)
# 4353               "newParser.ml"
 as _2))] -> Obj_formal_arg 
# 1080 "newParser.dyp"
(
                           ( seen_str dyp id; Formal_amp id ):'dypgen__Obj_formal_arg)
# 4358               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg,[Dyp.Ter Dyp_symbols.t_T_AMPER;Dyp.Ter Dyp_symbols.t_T_LID],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_AMPER  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4364               "newParser.ml"
 as _1));`Real_obj (Obj_T_LID  (
# 1082 "newParser.dyp"
                 (id,pos:string * Lexing.position)
# 4368               "newParser.ml"
 as _2))] -> Obj_formal_arg 
# 1081 "newParser.dyp"
(
                          ( seen_str dyp id; Formal_amp id ):'dypgen__Obj_formal_arg)
# 4373               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Ter Dyp_symbols.t_T_LID],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4379               "newParser.ml"
 as _1));`Real_obj (Obj_T_LID  (
# 1083 "newParser.dyp"
                 (id,pos:string * Lexing.position)
# 4383               "newParser.ml"
 as _2))] -> Obj_formal_arg 
# 1082 "newParser.dyp"
(
                           ( seen_str dyp id; Formal_star id):'dypgen__Obj_formal_arg)
# 4388               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg,[Dyp.Ter Dyp_symbols.t_T_USTAR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4394               "newParser.ml"
 as _1))] -> Obj_formal_arg 
# 1083 "newParser.dyp"
(
            (Formal_rest):'dypgen__Obj_formal_arg)
# 4399               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg,[Dyp.Ter Dyp_symbols.t_T_LID;Dyp.Ter Dyp_symbols.t_T_ASSIGN;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LID  (
# 1085 "newParser.dyp"
         (id,pos:string * Lexing.position)
# 4405               "newParser.ml"
 as _1));`Real_obj (Obj_T_ASSIGN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4409               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4413               "newParser.ml"
 as _3)));`Real_obj (Obj_arg ( (
# 1085 "newParser.dyp"
                                   (e:'dypgen__Obj_arg)
# 4417               "newParser.ml"
 as _4)))] -> Obj_formal_arg 
# 1084 "newParser.dyp"
(
                                       ( seen_str dyp id; Formal_default (id,e) ):'dypgen__Obj_formal_arg)
# 4422               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg,[Dyp.Non_ter (Dyp_symbols.lparen,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.formal_arg_nonempty_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_lparen ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_lparen)
# 4428               "newParser.ml"
 as _1)));`Real_obj (Obj_formal_arg_nonempty_list ( (
# 1086 "newParser.dyp"
                                   (f:'dypgen__Obj_formal_arg_nonempty_list)
# 4432               "newParser.ml"
 as _2)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4436               "newParser.ml"
 as _3))] -> Obj_formal_arg 
# 1085 "newParser.dyp"
(
                                                ( Formal_tuple f ):'dypgen__Obj_formal_arg)
# 4441               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg_nonempty_list,[Dyp.Non_ter (Dyp_symbols.formal_arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_formal_arg ( (
# 1089 "newParser.dyp"
              (f:'dypgen__Obj_formal_arg)
# 4447               "newParser.ml"
 as _1)))] -> Obj_formal_arg_nonempty_list 
# 1088 "newParser.dyp"
(
                  ( [f] ):'dypgen__Obj_formal_arg_nonempty_list)
# 4452               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg_nonempty_list,[Dyp.Non_ter (Dyp_symbols.formal_arg,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_formal_arg ( (
# 1090 "newParser.dyp"
              (f:'dypgen__Obj_formal_arg)
# 4458               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4462               "newParser.ml"
 as _2))] -> Obj_formal_arg_nonempty_list 
# 1089 "newParser.dyp"
(
                          ( [f;Formal_rest] ):'dypgen__Obj_formal_arg_nonempty_list)
# 4467               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg_nonempty_list,[Dyp.Non_ter (Dyp_symbols.formal_arg,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.formal_arg_nonempty_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_formal_arg ( (
# 1091 "newParser.dyp"
              (f:'dypgen__Obj_formal_arg)
# 4473               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4477               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4481               "newParser.ml"
 as _3)));`Real_obj (Obj_formal_arg_nonempty_list ( (
# 1091 "newParser.dyp"
                                                       (fs:'dypgen__Obj_formal_arg_nonempty_list)
# 4485               "newParser.ml"
 as _4)))] -> Obj_formal_arg_nonempty_list 
# 1091 "newParser.dyp"
(
      ( dyp.Dyp.will_shift <- false; f::fs ):'dypgen__Obj_formal_arg_nonempty_list)
# 4490               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg_list,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_formal_arg_list 
# 1094 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_formal_arg_list)
# 4497               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.formal_arg_list,[Dyp.Non_ter (Dyp_symbols.formal_arg_nonempty_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_formal_arg_nonempty_list ( (
# 1096 "newParser.dyp"
                            (l:'dypgen__Obj_formal_arg_nonempty_list)
# 4503               "newParser.ml"
 as _1)))] -> Obj_formal_arg_list 
# 1095 "newParser.dyp"
(
                                (l):'dypgen__Obj_formal_arg_list)
# 4508               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_formals,[Dyp.Non_ter (Dyp_symbols.eol_or_semi,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_eol_or_semi ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eol_or_semi)
# 4514               "newParser.ml"
 as _1)))] -> Obj_method_formals 
# 1098 "newParser.dyp"
(
                ( [] ):'dypgen__Obj_method_formals)
# 4519               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_formals,[Dyp.Non_ter (Dyp_symbols.formal_arg_nonempty_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eol_or_semi,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_formal_arg_nonempty_list ( (
# 1100 "newParser.dyp"
                            (formals:'dypgen__Obj_formal_arg_nonempty_list)
# 4525               "newParser.ml"
 as _1)));`Real_obj (Obj_eol_or_semi ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eol_or_semi)
# 4529               "newParser.ml"
 as _2)))] -> Obj_method_formals 
# 1100 "newParser.dyp"
(
      (match formals with
	  (Formal_tuple _)::_ -> raise Dyp.Giveup
	| e -> e
      ):'dypgen__Obj_method_formals)
# 4537               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_formals,[Dyp.Non_ter (Dyp_symbols.lparen,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.formal_arg_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_lparen ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_lparen)
# 4543               "newParser.ml"
 as _1)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4547               "newParser.ml"
 as _2)));`Real_obj (Obj_formal_arg_list ( (
# 1105 "newParser.dyp"
                               (formals:'dypgen__Obj_formal_arg_list)
# 4551               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4555               "newParser.ml"
 as _4)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4559               "newParser.ml"
 as _5))] -> Obj_method_formals 
# 1104 "newParser.dyp"
(
                                                       (
      (* RPAREN usually puts the lexer in an end state, but we need to
         force it to an end state *)
      state_override := true; formals):'dypgen__Obj_method_formals)
# 4567               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lhs,[Dyp.Non_ter (Dyp_symbols.scoped_id,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_scoped_id ( (
# 1111 "newParser.dyp"
             (id:'dypgen__Obj_scoped_id)
# 4573               "newParser.ml"
 as _1)))] -> Obj_lhs 
# 1110 "newParser.dyp"
(
                  ( id ):'dypgen__Obj_lhs)
# 4578               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lhs,[Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_LBRACK_ARG;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.arg_comma_star_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_primary ( (
# 1112 "newParser.dyp"
           (p:'dypgen__Obj_primary)
# 4584               "newParser.ml"
 as _1)));`Real_obj (Obj_T_LBRACK_ARG  (
# 1112 "newParser.dyp"
                           (pos:Lexing.position)
# 4588               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4592               "newParser.ml"
 as _3)));`Real_obj (Obj_arg_comma_star_list ( (
# 1112 "newParser.dyp"
                                                         (args:'dypgen__Obj_arg_comma_star_list)
# 4596               "newParser.ml"
 as _4)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4600               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4604               "newParser.ml"
 as _6))] -> Obj_lhs 
# 1112 "newParser.dyp"
(
      ( methodcall (E_Binop(p,Op_DOT,E_Operator(Op_AREF,pos),pos_of p)) args None (pos_of p)):'dypgen__Obj_lhs)
# 4609               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lhs,[Dyp.Non_ter (Dyp_symbols.primary,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_DOT;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.message_identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_primary ( (
# 1114 "newParser.dyp"
           (p:'dypgen__Obj_primary)
# 4615               "newParser.ml"
 as _1)));`Real_obj (Obj_T_DOT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4619               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4623               "newParser.ml"
 as _3)));`Real_obj (Obj_message_identifier ( (
# 1114 "newParser.dyp"
                                            (m:'dypgen__Obj_message_identifier)
# 4627               "newParser.ml"
 as _4)))] -> Obj_lhs 
# 1114 "newParser.dyp"
(
      ( methodcall (E_Binop(p,Op_DOT,m,pos_of p)) [] None (pos_of p)):'dypgen__Obj_lhs)
# 4632               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs,[Dyp.Non_ter (Dyp_symbols.mlhs_clean,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_mlhs_clean ( (
# 1118 "newParser.dyp"
              (ls:'dypgen__Obj_mlhs_clean)
# 4638               "newParser.ml"
 as _1)))] -> Obj_mlhs 
# 1117 "newParser.dyp"
(
                   (ls):'dypgen__Obj_mlhs)
# 4643               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs,[Dyp.Non_ter (Dyp_symbols.mlhs_clean,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_mlhs_clean ( (
# 1119 "newParser.dyp"
              (ls:'dypgen__Obj_mlhs_clean)
# 4649               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 1119 "newParser.dyp"
                          (pos:Lexing.position)
# 4653               "newParser.ml"
 as _2))] -> Obj_mlhs 
# 1118 "newParser.dyp"
(
                                ( ls @ [E_UOperator(Op_UStar,pos)] ):'dypgen__Obj_mlhs)
# 4658               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_clean,[Dyp.Ter Dyp_symbols.t_T_LPAREN;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.mlhs,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4664               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4668               "newParser.ml"
 as _2)));`Real_obj (Obj_mlhs ( (
# 1122 "newParser.dyp"
                      (l:'dypgen__Obj_mlhs)
# 4672               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4676               "newParser.ml"
 as _4)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4680               "newParser.ml"
 as _5))] -> Obj_mlhs_clean 
# 1121 "newParser.dyp"
(
                                        ( [(tuple l)] ):'dypgen__Obj_mlhs_clean)
# 4685               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_clean,[Dyp.Non_ter (Dyp_symbols.mlhs_item,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_mlhs_item ( (
# 1123 "newParser.dyp"
             (l:'dypgen__Obj_mlhs_item)
# 4691               "newParser.ml"
 as _1)))] -> Obj_mlhs_clean 
# 1122 "newParser.dyp"
(
                 ( [l] ):'dypgen__Obj_mlhs_clean)
# 4696               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_clean,[Dyp.Non_ter (Dyp_symbols.mlhs_item,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.mlhs_rest,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_mlhs_item ( (
# 1124 "newParser.dyp"
             (l:'dypgen__Obj_mlhs_item)
# 4702               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4706               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4710               "newParser.ml"
 as _3)));`Real_obj (Obj_mlhs_rest ( (
# 1124 "newParser.dyp"
                                       (ls:'dypgen__Obj_mlhs_rest)
# 4714               "newParser.ml"
 as _4)))] -> Obj_mlhs_clean 
# 1123 "newParser.dyp"
(
                                            ( l::ls):'dypgen__Obj_mlhs_clean)
# 4719               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_clean,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Non_ter (Dyp_symbols.lhs,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 1125 "newParser.dyp"
           (pos:Lexing.position)
# 4725               "newParser.ml"
 as _1));`Real_obj (Obj_lhs ( (
# 1125 "newParser.dyp"
                    (l:'dypgen__Obj_lhs)
# 4729               "newParser.ml"
 as _2)))] -> Obj_mlhs_clean 
# 1124 "newParser.dyp"
(
                        ( [E_Unary(Op_UStar,l,pos)] ):'dypgen__Obj_mlhs_clean)
# 4734               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_rest,[Dyp.Non_ter (Dyp_symbols.mlhs_item,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_mlhs_item ( (
# 1128 "newParser.dyp"
             (l:'dypgen__Obj_mlhs_item)
# 4740               "newParser.ml"
 as _1)))] -> Obj_mlhs_rest 
# 1127 "newParser.dyp"
(
                 ( [l] ):'dypgen__Obj_mlhs_rest)
# 4745               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_rest,[Dyp.Non_ter (Dyp_symbols.mlhs_item,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.mlhs_rest,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_mlhs_item ( (
# 1129 "newParser.dyp"
             (l:'dypgen__Obj_mlhs_item)
# 4751               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4755               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4759               "newParser.ml"
 as _3)));`Real_obj (Obj_mlhs_rest ( (
# 1129 "newParser.dyp"
                                       (ls:'dypgen__Obj_mlhs_rest)
# 4763               "newParser.ml"
 as _4)))] -> Obj_mlhs_rest 
# 1129 "newParser.dyp"
(
      ( dyp.Dyp.will_shift <- false; l::ls):'dypgen__Obj_mlhs_rest)
# 4768               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_rest,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Non_ter (Dyp_symbols.lhs,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 1131 "newParser.dyp"
           (pos:Lexing.position)
# 4774               "newParser.ml"
 as _1));`Real_obj (Obj_lhs ( (
# 1131 "newParser.dyp"
                    (l:'dypgen__Obj_lhs)
# 4778               "newParser.ml"
 as _2)))] -> Obj_mlhs_rest 
# 1130 "newParser.dyp"
(
                        ( [E_Unary(Op_UStar,l,pos)] ):'dypgen__Obj_mlhs_rest)
# 4783               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_item,[Dyp.Non_ter (Dyp_symbols.lhs,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_lhs ( (
# 1134 "newParser.dyp"
       (l:'dypgen__Obj_lhs)
# 4789               "newParser.ml"
 as _1)))] -> Obj_mlhs_item 
# 1133 "newParser.dyp"
(
           (l):'dypgen__Obj_mlhs_item)
# 4794               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_item,[Dyp.Ter Dyp_symbols.t_T_USTAR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 1135 "newParser.dyp"
           (pos:Lexing.position)
# 4800               "newParser.ml"
 as _1))] -> Obj_mlhs_item 
# 1134 "newParser.dyp"
(
                 ( E_UOperator(Op_UStar,pos) ):'dypgen__Obj_mlhs_item)
# 4805               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mlhs_item,[Dyp.Ter Dyp_symbols.t_T_LPAREN;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.mlhs,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4811               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4815               "newParser.ml"
 as _2)));`Real_obj (Obj_mlhs ( (
# 1136 "newParser.dyp"
                      (ls:'dypgen__Obj_mlhs)
# 4819               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4823               "newParser.ml"
 as _4)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4827               "newParser.ml"
 as _5))] -> Obj_mlhs_item 
# 1135 "newParser.dyp"
(
                                         ( tuple ls ):'dypgen__Obj_mlhs_item)
# 4832               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command_codeblock,[Dyp.Non_ter (Dyp_symbols.command,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command ( (
# 1139 "newParser.dyp"
           (c:'dypgen__Obj_command)
# 4838               "newParser.ml"
 as _1)))] -> Obj_command_codeblock 
# 1138 "newParser.dyp"
(
               ( c ):'dypgen__Obj_command_codeblock)
# 4843               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.command_codeblock,[Dyp.Non_ter (Dyp_symbols.command_name,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.code_block,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_command_name ( (
# 1140 "newParser.dyp"
                (c:'dypgen__Obj_command_name)
# 4849               "newParser.ml"
 as _1)));`Real_obj (Obj_code_block ( (
# 1140 "newParser.dyp"
                              (cb:'dypgen__Obj_code_block)
# 4853               "newParser.ml"
 as _2)))] -> Obj_command_codeblock 
# 1139 "newParser.dyp"
(
                                   ( command_codeblock c cb ):'dypgen__Obj_command_codeblock)
# 4858               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mrhs,[Dyp.Non_ter (Dyp_symbols.arg_comma_star_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_arg_comma_star_list ( (
# 1143 "newParser.dyp"
                       (args:'dypgen__Obj_arg_comma_star_list)
# 4864               "newParser.ml"
 as _1)))] -> Obj_mrhs 
# 1143 "newParser.dyp"
(
      ( match args with [] -> raise Dyp.Giveup | _ -> args):'dypgen__Obj_mrhs)
# 4869               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mrhs,[Dyp.Non_ter (Dyp_symbols.topcall,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_topcall ( (
# 1145 "newParser.dyp"
           (c:'dypgen__Obj_topcall)
# 4875               "newParser.ml"
 as _1)))] -> Obj_mrhs 
# 1144 "newParser.dyp"
(
               ( [c] ):'dypgen__Obj_mrhs)
# 4880               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mrhs,[Dyp.Ter Dyp_symbols.t_T_LBRACK;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LBRACK  (
# 1146 "newParser.dyp"
            (pos:Lexing.position)
# 4886               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4890               "newParser.ml"
 as _2)));`Real_obj (Obj_call_args ( (
# 1146 "newParser.dyp"
                                (r:'dypgen__Obj_call_args)
# 4894               "newParser.ml"
 as _3)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4898               "newParser.ml"
 as _4)));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4902               "newParser.ml"
 as _5))] -> Obj_mrhs 
# 1145 "newParser.dyp"
(
                                                  ( [E_Array(r,pos)] ):'dypgen__Obj_mrhs)
# 4907               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mrhs,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Non_ter (Dyp_symbols.arg,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 1147 "newParser.dyp"
           (pos:Lexing.position)
# 4913               "newParser.ml"
 as _1));`Real_obj (Obj_arg ( (
# 1147 "newParser.dyp"
                    (a:'dypgen__Obj_arg)
# 4917               "newParser.ml"
 as _2)))] -> Obj_mrhs 
# 1146 "newParser.dyp"
(
                        ( [E_Unary(Op_UStar,a,pos)] ):'dypgen__Obj_mrhs)
# 4922               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mrhs,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Non_ter (Dyp_symbols.command_codeblock,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 1148 "newParser.dyp"
           (pos:Lexing.position)
# 4928               "newParser.ml"
 as _1));`Real_obj (Obj_command_codeblock ( (
# 1148 "newParser.dyp"
                                  (c:'dypgen__Obj_command_codeblock)
# 4932               "newParser.ml"
 as _2)))] -> Obj_mrhs 
# 1147 "newParser.dyp"
(
                                      ( [E_Unary(Op_UStar,c,pos)] ):'dypgen__Obj_mrhs)
# 4937               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.mrhs,[Dyp.Ter Dyp_symbols.t_T_USTAR;Dyp.Ter Dyp_symbols.t_T_LBRACK;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.call_args,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USTAR  (
# 1150 "newParser.dyp"
           (pos1:Lexing.position)
# 4943               "newParser.ml"
 as _1));`Real_obj (Obj_T_LBRACK  (
# 1150 "newParser.dyp"
                          (pos2:Lexing.position)
# 4947               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4951               "newParser.ml"
 as _3)));`Real_obj (Obj_call_args ( (
# 1150 "newParser.dyp"
                                               (r:'dypgen__Obj_call_args)
# 4955               "newParser.ml"
 as _4)));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4959               "newParser.ml"
 as _5)));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4963               "newParser.ml"
 as _6))] -> Obj_mrhs 
# 1150 "newParser.dyp"
(
      ( [E_Unary(Op_UStar,E_Array(r,pos2),pos1)] ):'dypgen__Obj_mrhs)
# 4968               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.then_sep,[Dyp.Ter Dyp_symbols.t_T_SEMICOLON;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_SEMICOLON  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4974               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4978               "newParser.ml"
 as _2)))] -> Obj_then_sep 
# 1153 "newParser.dyp"
(
                       ():'dypgen__Obj_then_sep)
# 4983               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.then_sep,[Dyp.Ter Dyp_symbols.t_T_COLON;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_COLON  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 4989               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 4993               "newParser.ml"
 as _2)))] -> Obj_then_sep 
# 1154 "newParser.dyp"
(
                   ():'dypgen__Obj_then_sep)
# 4998               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.then_sep,[Dyp.Non_ter (Dyp_symbols.some_eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_some_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_some_eols)
# 5004               "newParser.ml"
 as _1)))] -> Obj_then_sep 
# 1155 "newParser.dyp"
(
                ():'dypgen__Obj_then_sep)
# 5009               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.then_sep,[Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_K_THEN;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5015               "newParser.ml"
 as _1)));`Real_obj (Obj_K_THEN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5019               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5023               "newParser.ml"
 as _3)))] -> Obj_then_sep 
# 1156 "newParser.dyp"
(
                       ():'dypgen__Obj_then_sep)
# 5028               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.when_clauses,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_when_clauses 
# 1159 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_when_clauses)
# 5035               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.when_clauses,[Dyp.Ter Dyp_symbols.t_K_WHEN;Dyp.Non_ter (Dyp_symbols.arg_comma_star_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.then_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.when_clauses,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_WHEN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5041               "newParser.ml"
 as _1));`Real_obj (Obj_arg_comma_star_list ( (
# 1161 "newParser.dyp"
                              (e:'dypgen__Obj_arg_comma_star_list)
# 5045               "newParser.ml"
 as _2)));`Real_obj (Obj_then_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_then_sep)
# 5049               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt_list ( (
# 1161 "newParser.dyp"
                                                    (es:'dypgen__Obj_stmt_list)
# 5053               "newParser.ml"
 as _4)));`Real_obj (Obj_when_clauses ( (
# 1161 "newParser.dyp"
                                                                     (rest:'dypgen__Obj_when_clauses)
# 5057               "newParser.ml"
 as _5)))] -> Obj_when_clauses 
# 1161 "newParser.dyp"
(
    ( (e,es) :: rest ):'dypgen__Obj_when_clauses)
# 5062               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.body_exn,[Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.rescue_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.case_else,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.ensure_clause,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_stmt_list ( (
# 1165 "newParser.dyp"
            (body:'dypgen__Obj_stmt_list)
# 5068               "newParser.ml"
 as _1)));`Real_obj (Obj_rescue_list ( (
# 1165 "newParser.dyp"
                              (resc_e:'dypgen__Obj_rescue_list)
# 5072               "newParser.ml"
 as _2)));`Real_obj (Obj_case_else ( (
# 1166 "newParser.dyp"
              (else_e:'dypgen__Obj_case_else)
# 5076               "newParser.ml"
 as _3)));`Real_obj (Obj_ensure_clause ( (
# 1166 "newParser.dyp"
                                    (ens_e:'dypgen__Obj_ensure_clause)
# 5080               "newParser.ml"
 as _4)))] -> Obj_body_exn 
# 1166 "newParser.dyp"
(
     ( {body_exprs = body;
	rescue_exprs = resc_e;
	ensure_expr = ens_e;
	else_expr = else_e} ):'dypgen__Obj_body_exn)
# 5088               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.rescue_clause,[Dyp.Ter Dyp_symbols.t_K_RESCUE;Dyp.Non_ter (Dyp_symbols.then_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RESCUE  (
# 1173 "newParser.dyp"
            (resc_pos:Lexing.position)
# 5094               "newParser.ml"
 as _1));`Real_obj (Obj_then_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_then_sep)
# 5098               "newParser.ml"
 as _2)));`Real_obj (Obj_stmt_list ( (
# 1173 "newParser.dyp"
                                         (body:'dypgen__Obj_stmt_list)
# 5102               "newParser.ml"
 as _3)))] -> Obj_rescue_clause 
# 1173 "newParser.dyp"
(
      ( let pos = match body with
	  | [] -> resc_pos 
	  | hd::_ -> pos_of hd
	in (E_Empty,mk_block body pos) ):'dypgen__Obj_rescue_clause)
# 5110               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.rescue_clause,[Dyp.Ter Dyp_symbols.t_K_RESCUE;Dyp.Ter Dyp_symbols.t_T_ASSOC;Dyp.Non_ter (Dyp_symbols.seen_id,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.then_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RESCUE  (
# 1179 "newParser.dyp"
            (pos:Lexing.position)
# 5116               "newParser.ml"
 as _1));`Real_obj (Obj_T_ASSOC  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5120               "newParser.ml"
 as _2));`Real_obj (Obj_seen_id ( (
# 1179 "newParser.dyp"
                                 (bind:'dypgen__Obj_seen_id)
# 5124               "newParser.ml"
 as _3)));`Real_obj (Obj_then_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_then_sep)
# 5128               "newParser.ml"
 as _4)));`Real_obj (Obj_stmt_list ( (
# 1179 "newParser.dyp"
                                                          (body:'dypgen__Obj_stmt_list)
# 5132               "newParser.ml"
 as _5)))] -> Obj_rescue_clause 
# 1179 "newParser.dyp"
(
      ( (E_Binop(E_Empty,Op_ASSOC,bind,pos), mk_block body pos) ):'dypgen__Obj_rescue_clause)
# 5137               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.rescue_clause,[Dyp.Ter Dyp_symbols.t_K_RESCUE;Dyp.Non_ter (Dyp_symbols.arg_comma_star_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.then_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RESCUE  (
# 1182 "newParser.dyp"
            (pos:Lexing.position)
# 5143               "newParser.ml"
 as _1));`Real_obj (Obj_arg_comma_star_list ( (
# 1182 "newParser.dyp"
                                     (exn:'dypgen__Obj_arg_comma_star_list)
# 5147               "newParser.ml"
 as _2)));`Real_obj (Obj_then_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_then_sep)
# 5151               "newParser.ml"
 as _3)));`Real_obj (Obj_stmt_list ( (
# 1182 "newParser.dyp"
                                                             (body:'dypgen__Obj_stmt_list)
# 5155               "newParser.ml"
 as _4)))] -> Obj_rescue_clause 
# 1182 "newParser.dyp"
(
    ( (tuple exn,mk_block body pos) ):'dypgen__Obj_rescue_clause)
# 5160               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.rescue_clause,[Dyp.Ter Dyp_symbols.t_K_RESCUE;Dyp.Non_ter (Dyp_symbols.arg_comma_star_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RESCUE  (
# 1185 "newParser.dyp"
            (pos:Lexing.position)
# 5166               "newParser.ml"
 as _1));`Real_obj (Obj_arg_comma_star_list ( (
# 1185 "newParser.dyp"
                                     (exn:'dypgen__Obj_arg_comma_star_list)
# 5170               "newParser.ml"
 as _2)))] -> Obj_rescue_clause 
# 1185 "newParser.dyp"
(
    ( (tuple exn,E_Empty) ):'dypgen__Obj_rescue_clause)
# 5175               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.eol_or_semi,[Dyp.Ter Dyp_symbols.t_T_SEMICOLON],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_SEMICOLON  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5181               "newParser.ml"
 as _1))] -> Obj_eol_or_semi 
# 1188 "newParser.dyp"
(
               ():'dypgen__Obj_eol_or_semi)
# 5186               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.eol_or_semi,[Dyp.Ter Dyp_symbols.t_T_EOL],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1] -> Obj_eol_or_semi 
# 1189 "newParser.dyp"
(
         ():'dypgen__Obj_eol_or_semi)
# 5193               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.rescue_list_rest,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_rescue_list_rest 
# 1192 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_rescue_list_rest)
# 5200               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.rescue_list_rest,[Dyp.Non_ter (Dyp_symbols.rescue_clause,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.rescue_list_rest,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_rescue_clause ( (
# 1194 "newParser.dyp"
                 (r:'dypgen__Obj_rescue_clause)
# 5206               "newParser.ml"
 as _1)));`Real_obj (Obj_rescue_list_rest ( (
# 1194 "newParser.dyp"
                                     (rs:'dypgen__Obj_rescue_list_rest)
# 5210               "newParser.ml"
 as _2)))] -> Obj_rescue_list_rest 
# 1193 "newParser.dyp"
(
                                          ( r::rs ):'dypgen__Obj_rescue_list_rest)
# 5215               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.rescue_list,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_rescue_list 
# 1196 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_rescue_list)
# 5222               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.rescue_list,[Dyp.Non_ter (Dyp_symbols.eol_or_semi,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.rescue_clause,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.rescue_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_eol_or_semi ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eol_or_semi)
# 5228               "newParser.ml"
 as _1)));`Real_obj (Obj_rescue_clause ( (
# 1199 "newParser.dyp"
                             (r:'dypgen__Obj_rescue_clause)
# 5232               "newParser.ml"
 as _2)));`Real_obj (Obj_rescue_list ( (
# 1199 "newParser.dyp"
                                            (rs:'dypgen__Obj_rescue_list)
# 5236               "newParser.ml"
 as _3)))] -> Obj_rescue_list 
# 1199 "newParser.dyp"
(
      ( r::rs ):'dypgen__Obj_rescue_list)
# 5241               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.case_else,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_case_else 
# 1202 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_case_else)
# 5248               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.case_else,[Dyp.Ter Dyp_symbols.t_K_ELSE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ELSE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5254               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5258               "newParser.ml"
 as _2)));`Real_obj (Obj_stmt_list ( (
# 1204 "newParser.dyp"
                         (ss:'dypgen__Obj_stmt_list)
# 5262               "newParser.ml"
 as _3)))] -> Obj_case_else 
# 1203 "newParser.dyp"
(
                              ( ss ):'dypgen__Obj_case_else)
# 5267               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.ensure_clause,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_ensure_clause 
# 1206 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_ensure_clause)
# 5274               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.ensure_clause,[Dyp.Ter Dyp_symbols.t_K_ENSURE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ENSURE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5280               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5284               "newParser.ml"
 as _2)));`Real_obj (Obj_stmt_list ( (
# 1208 "newParser.dyp"
                           (body:'dypgen__Obj_stmt_list)
# 5288               "newParser.ml"
 as _3)))] -> Obj_ensure_clause 
# 1207 "newParser.dyp"
(
                                  ( body ):'dypgen__Obj_ensure_clause)
# 5293               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.if_else_clauses,[],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_if_else_clauses 
# 1210 "newParser.dyp"
(
    ( [] ):'dypgen__Obj_if_else_clauses)
# 5300               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.if_else_clauses,[Dyp.Ter Dyp_symbols.t_K_ELSE;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ELSE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5306               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5310               "newParser.ml"
 as _2)));`Real_obj (Obj_stmt_list ( (
# 1212 "newParser.dyp"
                         (body:'dypgen__Obj_stmt_list)
# 5314               "newParser.ml"
 as _3)))] -> Obj_if_else_clauses 
# 1212 "newParser.dyp"
(
    ( body ):'dypgen__Obj_if_else_clauses)
# 5319               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.if_else_clauses,[Dyp.Ter Dyp_symbols.t_K_ELSIF;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.then_sep,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.if_else_clauses,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ELSIF  (
# 1214 "newParser.dyp"
           (pos:Lexing.position)
# 5325               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5329               "newParser.ml"
 as _2)));`Real_obj (Obj_expr ( (
# 1214 "newParser.dyp"
                          (guard:'dypgen__Obj_expr)
# 5333               "newParser.ml"
 as _3)));`Real_obj (Obj_then_sep ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_then_sep)
# 5337               "newParser.ml"
 as _4)));`Real_obj (Obj_stmt_list ( (
# 1214 "newParser.dyp"
                                                    (body:'dypgen__Obj_stmt_list)
# 5341               "newParser.ml"
 as _5)));`Real_obj (Obj_if_else_clauses ( (
# 1214 "newParser.dyp"
                                                                          (rest:'dypgen__Obj_if_else_clauses)
# 5345               "newParser.ml"
 as _6)))] -> Obj_if_else_clauses 
# 1214 "newParser.dyp"
(
    ( [E_If( guard, body, rest, pos) ] ):'dypgen__Obj_if_else_clauses)
# 5350               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.meth_or_atom,[Dyp.Non_ter (Dyp_symbols.method_name,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_method_name ( (
# 1218 "newParser.dyp"
                 (e:'dypgen__Obj_method_name)
# 5356               "newParser.ml"
 as _1)))] -> Obj_meth_or_atom 
# 1217 "newParser.dyp"
(
                      (e):'dypgen__Obj_meth_or_atom)
# 5361               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.meth_or_atom,[Dyp.Ter Dyp_symbols.t_T_ATOM],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_ATOM  (
# 1219 "newParser.dyp"
            (a,pos:Ast.interp_string * Lexing.position)
# 5367               "newParser.ml"
 as _1))] -> Obj_meth_or_atom 
# 1218 "newParser.dyp"
(
                    ( E_Literal((Lit_Atom a),pos) ):'dypgen__Obj_meth_or_atom)
# 5372               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.meth_or_atom_list,[Dyp.Non_ter (Dyp_symbols.meth_or_atom,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_meth_or_atom ( (
# 1222 "newParser.dyp"
                  (e:'dypgen__Obj_meth_or_atom)
# 5378               "newParser.ml"
 as _1)))] -> Obj_meth_or_atom_list 
# 1221 "newParser.dyp"
(
                      ( [e] ):'dypgen__Obj_meth_or_atom_list)
# 5383               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.meth_or_atom_list,[Dyp.Non_ter (Dyp_symbols.meth_or_atom,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_COMMA;Dyp.Non_ter (Dyp_symbols.meth_or_atom_list,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_meth_or_atom ( (
# 1223 "newParser.dyp"
                  (e:'dypgen__Obj_meth_or_atom)
# 5389               "newParser.ml"
 as _1)));`Real_obj (Obj_T_COMMA  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5393               "newParser.ml"
 as _2));`Real_obj (Obj_meth_or_atom_list ( (
# 1223 "newParser.dyp"
                                               (lst:'dypgen__Obj_meth_or_atom_list)
# 5397               "newParser.ml"
 as _3)))] -> Obj_meth_or_atom_list 
# 1222 "newParser.dyp"
(
                                                    ( e::lst ):'dypgen__Obj_meth_or_atom_list)
# 5402               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Non_ter (Dyp_symbols.message_identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_message_identifier ( (
# 1226 "newParser.dyp"
                      (e:'dypgen__Obj_message_identifier)
# 5408               "newParser.ml"
 as _1)))] -> Obj_method_name 
# 1225 "newParser.dyp"
(
                          ( e ):'dypgen__Obj_method_name)
# 5413               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Non_ter (Dyp_symbols.method_name,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_ASSIGN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_method_name ( (
# 1227 "newParser.dyp"
               (e:'dypgen__Obj_method_name)
# 5419               "newParser.ml"
 as _1)));`Real_obj (Obj_T_ASSIGN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5423               "newParser.ml"
 as _2))] -> Obj_method_name 
# 1226 "newParser.dyp"
(
                            ( add_eq e ):'dypgen__Obj_method_name)
# 5428               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Non_ter (Dyp_symbols.method_name,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_DOT;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.message_identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_method_name ( (
# 1228 "newParser.dyp"
               (id1:'dypgen__Obj_method_name)
# 5434               "newParser.ml"
 as _1)));`Real_obj (Obj_T_DOT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5438               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5442               "newParser.ml"
 as _3)));`Real_obj (Obj_message_identifier ( (
# 1228 "newParser.dyp"
                                                  (id2:'dypgen__Obj_message_identifier)
# 5446               "newParser.ml"
 as _4)))] -> Obj_method_name 
# 1228 "newParser.dyp"
(
      ( E_Binop(id1,Op_DOT,id2,pos_of id1) ):'dypgen__Obj_method_name)
# 5451               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Non_ter (Dyp_symbols.method_name,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_DOT;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.assignable,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_ASSIGN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_method_name ( (
# 1230 "newParser.dyp"
               (id1:'dypgen__Obj_method_name)
# 5457               "newParser.ml"
 as _1)));`Real_obj (Obj_T_DOT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5461               "newParser.ml"
 as _2));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5465               "newParser.ml"
 as _3)));`Real_obj (Obj_assignable ( (
# 1230 "newParser.dyp"
                                          (id2:'dypgen__Obj_assignable)
# 5469               "newParser.ml"
 as _4)));`Real_obj (Obj_T_ASSIGN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5473               "newParser.ml"
 as _5))] -> Obj_method_name 
# 1230 "newParser.dyp"
(
      ( E_Binop(id1,Op_DOT,add_eq id2,pos_of id1) ):'dypgen__Obj_method_name)
# 5478               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Ter Dyp_symbols.t_T_USCOPE;Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USCOPE  (
# 1233 "newParser.dyp"
            (pos:Lexing.position)
# 5484               "newParser.ml"
 as _1));`Real_obj (Obj_identifier ( (
# 1233 "newParser.dyp"
                            (id:'dypgen__Obj_identifier)
# 5488               "newParser.ml"
 as _2)))] -> Obj_method_name 
# 1232 "newParser.dyp"
(
                                     ( E_Unary(Op_UScope,id,pos)):'dypgen__Obj_method_name)
# 5493               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Ter Dyp_symbols.t_T_USCOPE;Dyp.Non_ter (Dyp_symbols.keyword_as_id,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USCOPE  (
# 1234 "newParser.dyp"
            (pos:Lexing.position)
# 5499               "newParser.ml"
 as _1));`Real_obj (Obj_keyword_as_id ( (
# 1234 "newParser.dyp"
                               (id:'dypgen__Obj_keyword_as_id)
# 5503               "newParser.ml"
 as _2)))] -> Obj_method_name 
# 1233 "newParser.dyp"
(
                                     ( E_Unary(Op_UScope,id,pos)):'dypgen__Obj_method_name)
# 5508               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Non_ter (Dyp_symbols.method_name,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_SCOPE;Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_method_name ( (
# 1236 "newParser.dyp"
               (id1:'dypgen__Obj_method_name)
# 5514               "newParser.ml"
 as _1)));`Real_obj (Obj_T_SCOPE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5518               "newParser.ml"
 as _2));`Real_obj (Obj_identifier ( (
# 1236 "newParser.dyp"
                                       (id2:'dypgen__Obj_identifier)
# 5522               "newParser.ml"
 as _3)))] -> Obj_method_name 
# 1235 "newParser.dyp"
(
                                                ( scope id1 id2 ):'dypgen__Obj_method_name)
# 5527               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Non_ter (Dyp_symbols.method_name,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_SCOPE;Dyp.Non_ter (Dyp_symbols.keyword_as_id,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_method_name ( (
# 1237 "newParser.dyp"
               (id1:'dypgen__Obj_method_name)
# 5533               "newParser.ml"
 as _1)));`Real_obj (Obj_T_SCOPE  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5537               "newParser.ml"
 as _2));`Real_obj (Obj_keyword_as_id ( (
# 1237 "newParser.dyp"
                                          (id2:'dypgen__Obj_keyword_as_id)
# 5541               "newParser.ml"
 as _3)))] -> Obj_method_name 
# 1236 "newParser.dyp"
(
                                                ( scope id1 id2 ):'dypgen__Obj_method_name)
# 5546               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.method_name,[Dyp.Ter Dyp_symbols.t_T_LPAREN;Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_RPAREN;Dyp.Ter Dyp_symbols.t_T_DOT;Dyp.Non_ter (Dyp_symbols.message_identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LPAREN  (
# 1238 "newParser.dyp"
            (pos:Lexing.position)
# 5552               "newParser.ml"
 as _1));`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 5556               "newParser.ml"
 as _2)));`Real_obj (Obj_stmt_list ( (
# 1238 "newParser.dyp"
                                (ss:'dypgen__Obj_stmt_list)
# 5560               "newParser.ml"
 as _3)));`Real_obj (Obj_T_RPAREN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5564               "newParser.ml"
 as _4));`Real_obj (Obj_T_DOT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5568               "newParser.ml"
 as _5));`Real_obj (Obj_message_identifier ( (
# 1238 "newParser.dyp"
                                                                      (m:'dypgen__Obj_message_identifier)
# 5572               "newParser.ml"
 as _6)))] -> Obj_method_name 
# 1238 "newParser.dyp"
(
      ( E_Binop(mk_block ss (pos_of (List.hd ss)), Op_DOT,m, pos) ):'dypgen__Obj_method_name)
# 5577               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.assignable,[Dyp.Non_ter (Dyp_symbols.identifier,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_identifier ( (
# 1242 "newParser.dyp"
              (e:'dypgen__Obj_identifier)
# 5583               "newParser.ml"
 as _1)))] -> Obj_assignable 
# 1241 "newParser.dyp"
(
                   ( e ):'dypgen__Obj_assignable)
# 5588               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.assignable,[Dyp.Non_ter (Dyp_symbols.keyword_as_id,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_keyword_as_id ( (
# 1243 "newParser.dyp"
                 (k:'dypgen__Obj_keyword_as_id)
# 5594               "newParser.ml"
 as _1)))] -> Obj_assignable 
# 1242 "newParser.dyp"
(
                     ( k ):'dypgen__Obj_assignable)
# 5599               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.assignable,[Dyp.Ter Dyp_symbols.t_T_LBRACK_ARG;Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LBRACK_ARG  (
# 1244 "newParser.dyp"
                (pos:Lexing.position)
# 5605               "newParser.ml"
 as _1));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5609               "newParser.ml"
 as _2))] -> Obj_assignable 
# 1243 "newParser.dyp"
(
                               ( E_Operator(Op_AREF,pos) ):'dypgen__Obj_assignable)
# 5614               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.assignable,[Dyp.Ter Dyp_symbols.t_T_LBRACK;Dyp.Ter Dyp_symbols.t_T_RBRACK],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LBRACK  (
# 1245 "newParser.dyp"
            (pos:Lexing.position)
# 5620               "newParser.ml"
 as _1));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5624               "newParser.ml"
 as _2))] -> Obj_assignable 
# 1244 "newParser.dyp"
(
                               ( E_Operator(Op_AREF,pos) ):'dypgen__Obj_assignable)
# 5629               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.message_identifier,[Dyp.Non_ter (Dyp_symbols.assignable,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_assignable ( (
# 1248 "newParser.dyp"
              (e:'dypgen__Obj_assignable)
# 5635               "newParser.ml"
 as _1)))] -> Obj_message_identifier 
# 1247 "newParser.dyp"
(
                  ( e ):'dypgen__Obj_message_identifier)
# 5640               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.message_identifier,[Dyp.Ter Dyp_symbols.t_T_LBRACK_ARG;Dyp.Ter Dyp_symbols.t_T_RBRACK;Dyp.Ter Dyp_symbols.t_T_ASSIGN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LBRACK_ARG  (
# 1249 "newParser.dyp"
                (pos:Lexing.position)
# 5646               "newParser.ml"
 as _1));`Real_obj (Obj_T_RBRACK  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5650               "newParser.ml"
 as _2));`Real_obj (Obj_T_ASSIGN  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5654               "newParser.ml"
 as _3))] -> Obj_message_identifier 
# 1248 "newParser.dyp"
(
                                        ( E_Operator(Op_ASET,pos) ):'dypgen__Obj_message_identifier)
# 5659               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.message_identifier,[Dyp.Non_ter (Dyp_symbols.bin_op,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_bin_op ( (
# 1250 "newParser.dyp"
          (op:'dypgen__Obj_bin_op)
# 5665               "newParser.ml"
 as _1)))] -> Obj_message_identifier 
# 1249 "newParser.dyp"
(
               ( E_Operator(op,Lexing.dummy_pos) ):'dypgen__Obj_message_identifier)
# 5670               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.message_identifier,[Dyp.Ter Dyp_symbols.t_T_UPLUS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_UPLUS  (
# 1251 "newParser.dyp"
           (pos:Lexing.position)
# 5676               "newParser.ml"
 as _1))] -> Obj_message_identifier 
# 1250 "newParser.dyp"
(
                       ( E_UOperator(Op_UPlus,pos) ):'dypgen__Obj_message_identifier)
# 5681               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.message_identifier,[Dyp.Ter Dyp_symbols.t_T_UMINUS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_UMINUS  (
# 1252 "newParser.dyp"
            (pos:Lexing.position)
# 5687               "newParser.ml"
 as _1))] -> Obj_message_identifier 
# 1251 "newParser.dyp"
(
                       ( E_UOperator(Op_UMinus,pos) ):'dypgen__Obj_message_identifier)
# 5692               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.message_identifier,[Dyp.Ter Dyp_symbols.t_T_TILDE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_TILDE  (
# 1253 "newParser.dyp"
           (pos:Lexing.position)
# 5698               "newParser.ml"
 as _1))] -> Obj_message_identifier 
# 1252 "newParser.dyp"
(
                       ( E_UOperator(Op_UTilde,pos) ):'dypgen__Obj_message_identifier)
# 5703               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_CMP],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_CMP  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5709               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1255 "newParser.dyp"
(
             ( Op_CMP ):'dypgen__Obj_bin_op)
# 5714               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_EQ],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_EQ  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5720               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1256 "newParser.dyp"
(
             ( Op_EQ ):'dypgen__Obj_bin_op)
# 5725               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_SLASH],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_SLASH  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5731               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1257 "newParser.dyp"
(
             ( Op_DIV ):'dypgen__Obj_bin_op)
# 5736               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_PERCENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_PERCENT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5742               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1258 "newParser.dyp"
(
             ( Op_REM ):'dypgen__Obj_bin_op)
# 5747               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_EQQ],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_EQQ  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5753               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1259 "newParser.dyp"
(
             ( Op_EQQ ):'dypgen__Obj_bin_op)
# 5758               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_NEQ],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_NEQ  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5764               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1260 "newParser.dyp"
(
             ( Op_NEQ ):'dypgen__Obj_bin_op)
# 5769               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_GEQ],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_GEQ  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5775               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1261 "newParser.dyp"
(
             ( Op_GEQ ):'dypgen__Obj_bin_op)
# 5780               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_LEQ],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LEQ  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5786               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1262 "newParser.dyp"
(
             ( Op_LEQ ):'dypgen__Obj_bin_op)
# 5791               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_MATCH],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_MATCH  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5797               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1263 "newParser.dyp"
(
             ( Op_MATCH ):'dypgen__Obj_bin_op)
# 5802               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_NMATCH],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_NMATCH  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5808               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1264 "newParser.dyp"
(
             ( Op_NMATCH ):'dypgen__Obj_bin_op)
# 5813               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_CARROT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_CARROT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5819               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1265 "newParser.dyp"
(
             ( Op_XOR ):'dypgen__Obj_bin_op)
# 5824               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_POW],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_POW  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5830               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1266 "newParser.dyp"
(
             ( Op_POW ):'dypgen__Obj_bin_op)
# 5835               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_VBAR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_VBAR  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5841               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1267 "newParser.dyp"
(
             ( Op_BOR ):'dypgen__Obj_bin_op)
# 5846               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_ASSOC],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_ASSOC  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5852               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1268 "newParser.dyp"
(
             ( Op_ASSOC ):'dypgen__Obj_bin_op)
# 5857               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_AMPER],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_AMPER  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5863               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1269 "newParser.dyp"
(
             ( Op_BAND ):'dypgen__Obj_bin_op)
# 5868               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_PLUS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_PLUS  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5874               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1270 "newParser.dyp"
(
             ( Op_PLUS ):'dypgen__Obj_bin_op)
# 5879               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_MINUS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_MINUS  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5885               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1271 "newParser.dyp"
(
             ( Op_MINUS ):'dypgen__Obj_bin_op)
# 5890               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_STAR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_STAR  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5896               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1272 "newParser.dyp"
(
             ( Op_TIMES ):'dypgen__Obj_bin_op)
# 5901               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_LSHFT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LSHFT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5907               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1273 "newParser.dyp"
(
             ( Op_LSHIFT ):'dypgen__Obj_bin_op)
# 5912               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_RSHFT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_RSHFT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5918               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1274 "newParser.dyp"
(
             ( Op_RSHIFT ):'dypgen__Obj_bin_op)
# 5923               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_LT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5929               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1275 "newParser.dyp"
(
             ( Op_LT ):'dypgen__Obj_bin_op)
# 5934               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_GT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_GT  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5940               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1276 "newParser.dyp"
(
             ( Op_GT ):'dypgen__Obj_bin_op)
# 5945               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_DOT2],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_DOT2  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5951               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1277 "newParser.dyp"
(
             ( Op_DOT2 ):'dypgen__Obj_bin_op)
# 5956               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.bin_op,[Dyp.Ter Dyp_symbols.t_T_DOT3],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_DOT3  (
# 0 "newParser.dyp"
(_:Lexing.position)
# 5962               "newParser.ml"
 as _1))] -> Obj_bin_op 
# 1278 "newParser.dyp"
(
             ( Op_DOT3 ):'dypgen__Obj_bin_op)
# 5967               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.unary_op,[Dyp.Ter Dyp_symbols.t_T_UPLUS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_UPLUS  (
# 1282 "newParser.dyp"
           (pos:Lexing.position)
# 5973               "newParser.ml"
 as _1))] -> Obj_unary_op 
# 1281 "newParser.dyp"
(
                   ( Op_UPlus,pos ):'dypgen__Obj_unary_op)
# 5978               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.unary_op,[Dyp.Ter Dyp_symbols.t_T_UMINUS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_UMINUS  (
# 1283 "newParser.dyp"
            (pos:Lexing.position)
# 5984               "newParser.ml"
 as _1))] -> Obj_unary_op 
# 1282 "newParser.dyp"
(
                   ( Op_UMinus,pos ):'dypgen__Obj_unary_op)
# 5989               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.unary_op,[Dyp.Ter Dyp_symbols.t_T_TILDE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_TILDE  (
# 1284 "newParser.dyp"
           (pos:Lexing.position)
# 5995               "newParser.ml"
 as _1))] -> Obj_unary_op 
# 1283 "newParser.dyp"
(
                   ( Op_UTilde,pos ):'dypgen__Obj_unary_op)
# 6000               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.unary_op,[Dyp.Ter Dyp_symbols.t_T_BANG],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_BANG  (
# 1285 "newParser.dyp"
          (pos:Lexing.position)
# 6006               "newParser.ml"
 as _1))] -> Obj_unary_op 
# 1284 "newParser.dyp"
(
                   ( Op_UBang,pos ):'dypgen__Obj_unary_op)
# 6011               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_CLASS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_CLASS  (
# 1288 "newParser.dyp"
           (t_info,pos:string*Lexing.position)
# 6017               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1287 "newParser.dyp"
(
                          ( E_Identifier(ID_Lowercase, "class", pos) ):'dypgen__Obj_keyword_as_id)
# 6022               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_MODULE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_MODULE  (
# 1289 "newParser.dyp"
            (t_info, pos:string*Lexing.position)
# 6028               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1288 "newParser.dyp"
(
                           ( E_Identifier(ID_Lowercase, "module", pos) ):'dypgen__Obj_keyword_as_id)
# 6033               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_DEF],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_DEF  (
# 1290 "newParser.dyp"
         (t_info,pos:string*Lexing.position)
# 6039               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1289 "newParser.dyp"
(
                          ( E_Identifier(ID_Lowercase, "def", pos) ):'dypgen__Obj_keyword_as_id)
# 6044               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_END],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_END  (
# 1291 "newParser.dyp"
         (pos:Lexing.position)
# 6050               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1290 "newParser.dyp"
(
                   ( E_Identifier(ID_Uppercase, "END", pos) ):'dypgen__Obj_keyword_as_id)
# 6055               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_ALIAS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ALIAS  (
# 1292 "newParser.dyp"
           (pos:Lexing.position)
# 6061               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1291 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "alias", pos) ):'dypgen__Obj_keyword_as_id)
# 6066               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_UNDEF],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_UNDEF  (
# 1293 "newParser.dyp"
           (pos:Lexing.position)
# 6072               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1292 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "undef", pos) ):'dypgen__Obj_keyword_as_id)
# 6077               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_BEGIN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_BEGIN  (
# 1294 "newParser.dyp"
           (pos:Lexing.position)
# 6083               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1293 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "BEGIN", pos) ):'dypgen__Obj_keyword_as_id)
# 6088               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_RESCUE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RESCUE  (
# 1295 "newParser.dyp"
            (pos:Lexing.position)
# 6094               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1294 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "rescue", pos) ):'dypgen__Obj_keyword_as_id)
# 6099               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_ENSURE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ENSURE  (
# 1296 "newParser.dyp"
            (pos:Lexing.position)
# 6105               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1295 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "ensure", pos) ):'dypgen__Obj_keyword_as_id)
# 6110               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_IF],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_IF  (
# 1297 "newParser.dyp"
        (pos:Lexing.position)
# 6116               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1296 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "if", pos) ):'dypgen__Obj_keyword_as_id)
# 6121               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_UNLESS],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_UNLESS  (
# 1298 "newParser.dyp"
            (pos:Lexing.position)
# 6127               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1297 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "unless", pos) ):'dypgen__Obj_keyword_as_id)
# 6132               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_THEN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_THEN  (
# 1299 "newParser.dyp"
          (pos:Lexing.position)
# 6138               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1298 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "then", pos) ):'dypgen__Obj_keyword_as_id)
# 6143               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_ELSIF],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ELSIF  (
# 1300 "newParser.dyp"
           (pos:Lexing.position)
# 6149               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1299 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "elsif", pos) ):'dypgen__Obj_keyword_as_id)
# 6154               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_ELSE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_ELSE  (
# 1301 "newParser.dyp"
          (pos:Lexing.position)
# 6160               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1300 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "else", pos) ):'dypgen__Obj_keyword_as_id)
# 6165               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_CASE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_CASE  (
# 1302 "newParser.dyp"
          (pos:Lexing.position)
# 6171               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1301 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "case", pos) ):'dypgen__Obj_keyword_as_id)
# 6176               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_WHEN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_WHEN  (
# 1303 "newParser.dyp"
          (pos:Lexing.position)
# 6182               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1302 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "when", pos) ):'dypgen__Obj_keyword_as_id)
# 6187               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_WHILE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_WHILE  (
# 1304 "newParser.dyp"
           (pos:Lexing.position)
# 6193               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1303 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "while", pos) ):'dypgen__Obj_keyword_as_id)
# 6198               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_UNTIL],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_UNTIL  (
# 1305 "newParser.dyp"
           (pos:Lexing.position)
# 6204               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1304 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "until", pos) ):'dypgen__Obj_keyword_as_id)
# 6209               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_FOR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_FOR  (
# 1306 "newParser.dyp"
         (pos:Lexing.position)
# 6215               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1305 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "for", pos) ):'dypgen__Obj_keyword_as_id)
# 6220               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_IN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_IN  (
# 1307 "newParser.dyp"
        (pos:Lexing.position)
# 6226               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1306 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "in", pos) ):'dypgen__Obj_keyword_as_id)
# 6231               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_DO],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_DO  (
# 1308 "newParser.dyp"
        (pos:Lexing.position)
# 6237               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1307 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "do", pos) ):'dypgen__Obj_keyword_as_id)
# 6242               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_RETURN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_RETURN  (
# 1309 "newParser.dyp"
            (pos:Lexing.position)
# 6248               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1308 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "return", pos) ):'dypgen__Obj_keyword_as_id)
# 6253               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_AND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_AND  (
# 1310 "newParser.dyp"
         (pos:Lexing.position)
# 6259               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1309 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "and", pos) ):'dypgen__Obj_keyword_as_id)
# 6264               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_OR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_OR  (
# 1311 "newParser.dyp"
        (pos:Lexing.position)
# 6270               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1310 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "or", pos) ):'dypgen__Obj_keyword_as_id)
# 6275               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_NOT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_NOT  (
# 1312 "newParser.dyp"
         (pos:Lexing.position)
# 6281               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1311 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "not", pos) ):'dypgen__Obj_keyword_as_id)
# 6286               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_lBEGIN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_lBEGIN  (
# 1314 "newParser.dyp"
            (pos:Lexing.position)
# 6292               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1313 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "begin", pos) ):'dypgen__Obj_keyword_as_id)
# 6297               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_lEND],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_lEND  (
# 1315 "newParser.dyp"
          (pos:Lexing.position)
# 6303               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1314 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "end", pos) ):'dypgen__Obj_keyword_as_id)
# 6308               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.keyword_as_id,[Dyp.Ter Dyp_symbols.t_K_YIELD],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_YIELD  (
# 1316 "newParser.dyp"
           (pos:Lexing.position)
# 6314               "newParser.ml"
 as _1))] -> Obj_keyword_as_id 
# 1315 "newParser.dyp"
(
                   ( E_Identifier(ID_Lowercase, "yield", pos) ):'dypgen__Obj_keyword_as_id)
# 6319               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_T_BUILTIN_VAR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_BUILTIN_VAR  (
# 1319 "newParser.dyp"
                 (id,pos:string * Lexing.position)
# 6325               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1318 "newParser.dyp"
(
                          ( E_Identifier(ID_Builtin, id, pos) ):'dypgen__Obj_identifier)
# 6330               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_T_GLOBAL_VAR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_GLOBAL_VAR  (
# 1320 "newParser.dyp"
                (id,pos:string * Lexing.position)
# 6336               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1319 "newParser.dyp"
(
                          ( E_Identifier (ID_Global, id, pos) ):'dypgen__Obj_identifier)
# 6341               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_T_UID],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_UID  (
# 1321 "newParser.dyp"
         (id,pos:string * Lexing.position)
# 6347               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1320 "newParser.dyp"
(
                   ( E_Identifier (ID_Uppercase, id, pos)):'dypgen__Obj_identifier)
# 6352               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_K_NIL],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_NIL  (
# 1322 "newParser.dyp"
         (pos:Lexing.position)
# 6358               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1321 "newParser.dyp"
(
                   ( E_Literal(Lit_Nil,pos) ):'dypgen__Obj_identifier)
# 6363               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_K_SELF],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_SELF  (
# 1323 "newParser.dyp"
          (pos:Lexing.position)
# 6369               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1322 "newParser.dyp"
(
                   ( E_Literal(Lit_Self,pos) ):'dypgen__Obj_identifier)
# 6374               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_K_TRUE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_TRUE  (
# 1324 "newParser.dyp"
          (pos:Lexing.position)
# 6380               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1323 "newParser.dyp"
(
                   ( E_Literal(Lit_True,pos) ):'dypgen__Obj_identifier)
# 6385               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_K_FALSE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_K_FALSE  (
# 1325 "newParser.dyp"
           (pos:Lexing.position)
# 6391               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1324 "newParser.dyp"
(
                   ( E_Literal(Lit_False,pos) ):'dypgen__Obj_identifier)
# 6396               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_T_LID],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LID  (
# 1326 "newParser.dyp"
         (id,pos:string * Lexing.position)
# 6402               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1325 "newParser.dyp"
(
                   ( E_Identifier (ID_Lowercase, id, pos)):'dypgen__Obj_identifier)
# 6407               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_T_INST_VAR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_INST_VAR  (
# 1327 "newParser.dyp"
              (id,pos:string * Lexing.position)
# 6413               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1326 "newParser.dyp"
(
                          ( E_Identifier(ID_Instance, id, pos) ):'dypgen__Obj_identifier)
# 6418               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.identifier,[Dyp.Ter Dyp_symbols.t_T_CLASS_VAR],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_CLASS_VAR  (
# 1328 "newParser.dyp"
               (id,pos:string * Lexing.position)
# 6424               "newParser.ml"
 as _1))] -> Obj_identifier 
# 1327 "newParser.dyp"
(
                           ( E_Identifier(ID_Class, id, pos) ):'dypgen__Obj_identifier)
# 6429               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.one_string,[Dyp.Ter Dyp_symbols.t_T_SINGLE_STRING],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_SINGLE_STRING  (
# 1331 "newParser.dyp"
                   (s,pos:string * Lexing.position)
# 6435               "newParser.ml"
 as _1))] -> Obj_one_string 
# 1330 "newParser.dyp"
(
                           ( E_Literal( Lit_String(String_Single s), pos) ):'dypgen__Obj_one_string)
# 6440               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.one_string,[Dyp.Ter Dyp_symbols.t_T_DOUBLE_BEG;Dyp.Non_ter (Dyp_symbols.interp_str,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_DOUBLE_BEG  (
# 1332 "newParser.dyp"
                (pos:Lexing.position)
# 6446               "newParser.ml"
 as _1));`Real_obj (Obj_interp_str ( (
# 1332 "newParser.dyp"
                                (istr:'dypgen__Obj_interp_str)
# 6450               "newParser.ml"
 as _2)))] -> Obj_one_string 
# 1332 "newParser.dyp"
(
      ( E_Literal(Lit_String (String_Double istr),pos) ):'dypgen__Obj_one_string)
# 6455               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.one_string,[Dyp.Ter Dyp_symbols.t_T_DOUBLE_STRING],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_DOUBLE_STRING  (
# 1335 "newParser.dyp"
                   (s,pos:Ast.interp_string * Lexing.position)
# 6461               "newParser.ml"
 as _1))] -> Obj_one_string 
# 1334 "newParser.dyp"
(
                           ( E_Literal( Lit_String(String_Double s), pos) ):'dypgen__Obj_one_string)
# 6466               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.one_string,[Dyp.Ter Dyp_symbols.t_T_USER_STRING],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USER_STRING  (
# 1336 "newParser.dyp"
                 (m,str,pos:string * Ast.interp_string * Lexing.position)
# 6472               "newParser.ml"
 as _1))] -> Obj_one_string 
# 1335 "newParser.dyp"
(
                             ( process_user_string m str pos ):'dypgen__Obj_one_string)
# 6477               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.one_string,[Dyp.Ter Dyp_symbols.t_T_USER_BEG;Dyp.Non_ter (Dyp_symbols.interp_str,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_USER_BEG  (
# 1337 "newParser.dyp"
              (m,pos:string * Lexing.position)
# 6483               "newParser.ml"
 as _1));`Real_obj (Obj_interp_str ( (
# 1337 "newParser.dyp"
                                (str:'dypgen__Obj_interp_str)
# 6487               "newParser.ml"
 as _2)))] -> Obj_one_string 
# 1336 "newParser.dyp"
(
                                      ( process_user_string m str pos ):'dypgen__Obj_one_string)
# 6492               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.string,[Dyp.Non_ter (Dyp_symbols.one_string,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_one_string ( (
# 1340 "newParser.dyp"
              (s:'dypgen__Obj_one_string)
# 6498               "newParser.ml"
 as _1)))] -> Obj_string 
# 1339 "newParser.dyp"
(
                  (s):'dypgen__Obj_string)
# 6503               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.string,[Dyp.Non_ter (Dyp_symbols.string,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.one_string,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_string ( (
# 1342 "newParser.dyp"
          (s1:'dypgen__Obj_string)
# 6509               "newParser.ml"
 as _1)));`Real_obj (Obj_one_string ( (
# 1342 "newParser.dyp"
                         (s2:'dypgen__Obj_one_string)
# 6513               "newParser.ml"
 as _2)))] -> Obj_string 
# 1341 "newParser.dyp"
(
                              ( merge_string_lits s1 s2 ):'dypgen__Obj_string)
# 6518               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Non_ter (Dyp_symbols.string,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_string ( (
# 1345 "newParser.dyp"
          (s:'dypgen__Obj_string)
# 6524               "newParser.ml"
 as _1)))] -> Obj_constant 
# 1344 "newParser.dyp"
(
              ( s ):'dypgen__Obj_constant)
# 6529               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Ter Dyp_symbols.t_T_TICK_BEG;Dyp.Non_ter (Dyp_symbols.interp_str,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_TICK_BEG  (
# 1347 "newParser.dyp"
              (pos:Lexing.position)
# 6535               "newParser.ml"
 as _1));`Real_obj (Obj_interp_str ( (
# 1347 "newParser.dyp"
                              (istr:'dypgen__Obj_interp_str)
# 6539               "newParser.ml"
 as _2)))] -> Obj_constant 
# 1347 "newParser.dyp"
(
      ( E_Literal (Lit_String (String_Tick istr),pos) ):'dypgen__Obj_constant)
# 6544               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Ter Dyp_symbols.t_T_ATOM_BEG;Dyp.Non_ter (Dyp_symbols.interp_str,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_ATOM_BEG  (
# 1350 "newParser.dyp"
              (pos:Lexing.position)
# 6550               "newParser.ml"
 as _1));`Real_obj (Obj_interp_str ( (
# 1350 "newParser.dyp"
                              (istr:'dypgen__Obj_interp_str)
# 6554               "newParser.ml"
 as _2)))] -> Obj_constant 
# 1350 "newParser.dyp"
(
      ( E_Literal (Lit_Atom istr,pos) ):'dypgen__Obj_constant)
# 6559               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Ter Dyp_symbols.t_T_REGEXP_BEG;Dyp.Non_ter (Dyp_symbols.interp_str,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_T_REGEXP_MOD],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_REGEXP_BEG  (
# 1353 "newParser.dyp"
                (pos:Lexing.position)
# 6565               "newParser.ml"
 as _1));`Real_obj (Obj_interp_str ( (
# 1353 "newParser.dyp"
                                (istr:'dypgen__Obj_interp_str)
# 6569               "newParser.ml"
 as _2)));`Real_obj (Obj_T_REGEXP_MOD  (
# 1353 "newParser.dyp"
                                                   (mods:string)
# 6573               "newParser.ml"
 as _3))] -> Obj_constant 
# 1353 "newParser.dyp"
(
      ( E_Literal(Lit_Regexp (istr,mods),pos) ):'dypgen__Obj_constant)
# 6578               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Ter Dyp_symbols.t_T_REGEXP],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_REGEXP  (
# 1355 "newParser.dyp"
            (s,m,pos:Ast.interp_string * string * Lexing.position)
# 6584               "newParser.ml"
 as _1))] -> Obj_constant 
# 1354 "newParser.dyp"
(
                      ( E_Literal(Lit_Regexp (s,m),pos) ):'dypgen__Obj_constant)
# 6589               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Ter Dyp_symbols.t_T_FIXNUM],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_FIXNUM  (
# 1357 "newParser.dyp"
            (i,pos:int * Lexing.position)
# 6595               "newParser.ml"
 as _1))] -> Obj_constant 
# 1356 "newParser.dyp"
(
                    ( E_Literal(Lit_FixNum i,pos) ):'dypgen__Obj_constant)
# 6600               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Ter Dyp_symbols.t_T_BIGNUM],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_BIGNUM  (
# 1358 "newParser.dyp"
            (i,pos:Big_int.big_int * Lexing.position)
# 6606               "newParser.ml"
 as _1))] -> Obj_constant 
# 1357 "newParser.dyp"
(
                    ( E_Literal(Lit_BigNum i,pos) ):'dypgen__Obj_constant)
# 6611               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Ter Dyp_symbols.t_T_FLOAT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_FLOAT  (
# 1359 "newParser.dyp"
           (s,f,pos:string * float * Lexing.position)
# 6617               "newParser.ml"
 as _1))] -> Obj_constant 
# 1358 "newParser.dyp"
(
                     ( E_Literal((Lit_Float(s,f)),pos) ):'dypgen__Obj_constant)
# 6622               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.constant,[Dyp.Ter Dyp_symbols.t_T_ATOM],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_ATOM  (
# 1360 "newParser.dyp"
          (a,pos:Ast.interp_string * Lexing.position)
# 6628               "newParser.ml"
 as _1))] -> Obj_constant 
# 1359 "newParser.dyp"
(
                  ( E_Literal (Lit_Atom a,pos) ):'dypgen__Obj_constant)
# 6633               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.interp_str,[Dyp.Ter Dyp_symbols.t_T_SINGLE_STRING],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_SINGLE_STRING  (
# 1363 "newParser.dyp"
                   (str,p:string * Lexing.position)
# 6639               "newParser.ml"
 as _1))] -> Obj_interp_str 
# 1362 "newParser.dyp"
(
                           ([StrChars str]):'dypgen__Obj_interp_str)
# 6644               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.interp_str,[Dyp.Non_ter (Dyp_symbols.interp_str_work,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_interp_str_work ( (
# 1364 "newParser.dyp"
                   (i:'dypgen__Obj_interp_str_work)
# 6650               "newParser.ml"
 as _1)))] -> Obj_interp_str 
# 1364 "newParser.dyp"
(
      (match i with [] -> [StrChars ""] | lst -> lst):'dypgen__Obj_interp_str)
# 6655               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.interp_str_work,[Dyp.Ter Dyp_symbols.t_T_INTERP_STR;Dyp.Non_ter (Dyp_symbols.interp_code,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_INTERP_STR  (
# 1368 "newParser.dyp"
                (s,pos:string * Lexing.position)
# 6661               "newParser.ml"
 as _1));`Real_obj (Obj_interp_code ( (
# 1368 "newParser.dyp"
                                   (tl:'dypgen__Obj_interp_code)
# 6665               "newParser.ml"
 as _2)))] -> Obj_interp_str_work 
# 1368 "newParser.dyp"
(
      ( if s = "" then tl else (StrChars s)::tl ):'dypgen__Obj_interp_str_work)
# 6670               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.interp_str_work,[Dyp.Ter Dyp_symbols.t_T_INTERP_END],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_INTERP_END  (
# 1371 "newParser.dyp"
                (s,pos2:string * Lexing.position)
# 6676               "newParser.ml"
 as _1))] -> Obj_interp_str_work 
# 1371 "newParser.dyp"
(
      ( if s = "" then [] else [StrChars s] ):'dypgen__Obj_interp_str_work)
# 6681               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.interp_code,[Dyp.Non_ter (Dyp_symbols.eols,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.stmt_list,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.interp_str_work,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_eols ( (
# 0 "newParser.dyp"
(_:'dypgen__Obj_eols)
# 6687               "newParser.ml"
 as _1)));`Real_obj (Obj_stmt_list ( (
# 1375 "newParser.dyp"
                  (ss:'dypgen__Obj_stmt_list)
# 6691               "newParser.ml"
 as _2)));`Real_obj (Obj_interp_str_work ( (
# 1375 "newParser.dyp"
                                      (tl:'dypgen__Obj_interp_str_work)
# 6695               "newParser.ml"
 as _3)))] -> Obj_interp_code 
# 1375 "newParser.dyp"
(
      ( match ss with
          | [] -> tl
          | x::_ -> StrExpr (mk_block ss (pos_of x))::tl ):'dypgen__Obj_interp_code)
# 6702               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.interp_code,[Dyp.Ter Dyp_symbols.t_T_INTERP_END],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_INTERP_END  (
# 1380 "newParser.dyp"
                (s,pos:string * Lexing.position)
# 6708               "newParser.ml"
 as _1))] -> Obj_interp_code 
# 1380 "newParser.dyp"
(
      ( if s = "" then [] else [StrChars s] ):'dypgen__Obj_interp_code)
# 6713               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lparen,[Dyp.Ter Dyp_symbols.t_T_LPAREN],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LPAREN  (
# 1384 "newParser.dyp"
            (pos:Lexing.position)
# 6719               "newParser.ml"
 as _1))] -> Obj_lparen 
# 1383 "newParser.dyp"
(
                  (pos):'dypgen__Obj_lparen)
# 6724               "newParser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lparen,[Dyp.Ter Dyp_symbols.t_T_LPAREN_ARG],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_T_LPAREN_ARG  (
# 1385 "newParser.dyp"
                (pos:Lexing.position)
# 6730               "newParser.ml"
 as _1))] -> Obj_lparen 
# 1384 "newParser.dyp"
(
                      (pos):'dypgen__Obj_lparen)
# 6735               "newParser.ml"
 | _ -> raise Dyp.Giveup)))]

let dyp_merge_arg ol o =
  let ol2 = dyp_merge_arg ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_arg_comma_list_trail ol o =
  let ol2 = dyp_merge_arg_comma_list_trail ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_arg_comma_nonempty_list ol o =
  let ol2 = dyp_merge_arg_comma_nonempty_list ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_arg_comma_star_list ol o =
  let ol2 = dyp_merge_arg_comma_star_list ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_array_body ol o =
  let ol2 = dyp_merge_array_body ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_array_body_rest ol o =
  let ol2 = dyp_merge_array_body_rest ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_array_item ol o =
  let ol2 = dyp_merge_array_item ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_assign_op ol o =
  let ol2 = dyp_merge_assign_op ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_assignable ol o =
  let ol2 = dyp_merge_assignable ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_bin_op ol o =
  let ol2 = dyp_merge_bin_op ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_body_exn ol o =
  let ol2 = dyp_merge_body_exn ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_brace_codeblock ol o =
  let ol2 = dyp_merge_brace_codeblock ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_call_args ol o =
  let ol2 = dyp_merge_call_args ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_case_else ol o =
  let ol2 = dyp_merge_case_else ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_class_inheritance ol o =
  let ol2 = dyp_merge_class_inheritance ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_code_block ol o =
  let ol2 = dyp_merge_code_block ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_code_block_body ol o =
  let ol2 = dyp_merge_code_block_body ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_command ol o =
  let ol2 = dyp_merge_command ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_command_codeblock ol o =
  let ol2 = dyp_merge_command_codeblock ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_command_name ol o =
  let ol2 = dyp_merge_command_name ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_constant ol o =
  let ol2 = dyp_merge_constant ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_do_codeblock ol o =
  let ol2 = dyp_merge_do_codeblock ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_do_sep ol o =
  let ol2 = dyp_merge_do_sep ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_ensure_clause ol o =
  let ol2 = dyp_merge_ensure_clause ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_eol_or_semi ol o =
  let ol2 = dyp_merge_eol_or_semi ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_eols ol o =
  let ol2 = dyp_merge_eols ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_expr ol o =
  let ol2 = dyp_merge_expr ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_formal_arg ol o =
  let ol2 = dyp_merge_formal_arg ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_formal_arg_list ol o =
  let ol2 = dyp_merge_formal_arg_list ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_formal_arg_nonempty_list ol o =
  let ol2 = dyp_merge_formal_arg_nonempty_list ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_func ol o =
  let ol2 = dyp_merge_func ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_identifier ol o =
  let ol2 = dyp_merge_identifier ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_if_else_clauses ol o =
  let ol2 = dyp_merge_if_else_clauses ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_interp_code ol o =
  let ol2 = dyp_merge_interp_code ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_interp_str ol o =
  let ol2 = dyp_merge_interp_str ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_interp_str_work ol o =
  let ol2 = dyp_merge_interp_str_work ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_keyword_as_id ol o =
  let ol2 = dyp_merge_keyword_as_id ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_lhs ol o =
  let ol2 = dyp_merge_lhs ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_lhs_assign_op ol o =
  let ol2 = dyp_merge_lhs_assign_op ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_lhs_prune_binop ol o =
  let ol2 = dyp_merge_lhs_prune_binop ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_lhs_pruned_assign_op ol o =
  let ol2 = dyp_merge_lhs_pruned_assign_op ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_lparen ol o =
  let ol2 = dyp_merge_lparen ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_main ol o =
  let ol2 = dyp_merge_main ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_message_identifier ol o =
  let ol2 = dyp_merge_message_identifier ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_meth_or_atom ol o =
  let ol2 = dyp_merge_meth_or_atom ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_meth_or_atom_list ol o =
  let ol2 = dyp_merge_meth_or_atom_list ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_method_formals ol o =
  let ol2 = dyp_merge_method_formals ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_method_name ol o =
  let ol2 = dyp_merge_method_name ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_mlhs ol o =
  let ol2 = dyp_merge_mlhs ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_mlhs_assign_op ol o =
  let ol2 = dyp_merge_mlhs_assign_op ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_mlhs_clean ol o =
  let ol2 = dyp_merge_mlhs_clean ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_mlhs_item ol o =
  let ol2 = dyp_merge_mlhs_item ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_mlhs_rest ol o =
  let ol2 = dyp_merge_mlhs_rest ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_mrhs ol o =
  let ol2 = dyp_merge_mrhs ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_one_string ol o =
  let ol2 = dyp_merge_one_string ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_primary ol o =
  let ol2 = dyp_merge_primary ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_rescue_clause ol o =
  let ol2 = dyp_merge_rescue_clause ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_rescue_list ol o =
  let ol2 = dyp_merge_rescue_list ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_rescue_list_rest ol o =
  let ol2 = dyp_merge_rescue_list_rest ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_scope_begin ol o =
  let ol2 = dyp_merge_scope_begin ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_scope_class ol o =
  let ol2 = dyp_merge_scope_class ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_scope_def ol o =
  let ol2 = dyp_merge_scope_def ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_scope_end ol o =
  let ol2 = dyp_merge_scope_end ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_scope_module ol o =
  let ol2 = dyp_merge_scope_module ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_scoped_id ol o =
  let ol2 = dyp_merge_scoped_id ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_seen_id ol o =
  let ol2 = dyp_merge_seen_id ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_some_eols ol o =
  let ol2 = dyp_merge_some_eols ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_star_amper ol o =
  let ol2 = dyp_merge_star_amper ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_stmt ol o =
  let ol2 = dyp_merge_stmt ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_stmt_list ol o =
  let ol2 = dyp_merge_stmt_list ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_string ol o =
  let ol2 = dyp_merge_string ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_then_sep ol o =
  let ol2 = dyp_merge_then_sep ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_topcall ol o =
  let ol2 = dyp_merge_topcall ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_unary_op ol o =
  let ol2 = dyp_merge_unary_op ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_when_clauses ol o =
  let ol2 = dyp_merge_when_clauses ol o in
  if ol2 = [] then dyp_merge ol o else ol2

let __dypgen_merge_map = Dyp_runtime.Tools.init_merge_map [(fun ol o -> (
  let f1 o = match o with Obj_when_clauses ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_when_clauses"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_when_clauses ol o in
  let f2 o = Obj_when_clauses o in
  List.map f2 ol)),75;(fun ol o -> (
  let f1 o = match o with Obj_unary_op ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_unary_op"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_unary_op ol o in
  let f2 o = Obj_unary_op o in
  List.map f2 ol)),74;(fun ol o -> (
  let f1 o = match o with Obj_topcall ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_topcall"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_topcall ol o in
  let f2 o = Obj_topcall o in
  List.map f2 ol)),73;(fun ol o -> (
  let f1 o = match o with Obj_then_sep ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_then_sep"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_then_sep ol o in
  let f2 o = Obj_then_sep o in
  List.map f2 ol)),72;(fun ol o -> (
  let f1 o = match o with Obj_string ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_string"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_string ol o in
  let f2 o = Obj_string o in
  List.map f2 ol)),71;(fun ol o -> (
  let f1 o = match o with Obj_stmt_list ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_stmt_list"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_stmt_list ol o in
  let f2 o = Obj_stmt_list o in
  List.map f2 ol)),70;(fun ol o -> (
  let f1 o = match o with Obj_stmt ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_stmt"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_stmt ol o in
  let f2 o = Obj_stmt o in
  List.map f2 ol)),69;(fun ol o -> (
  let f1 o = match o with Obj_star_amper ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_star_amper"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_star_amper ol o in
  let f2 o = Obj_star_amper o in
  List.map f2 ol)),68;(fun ol o -> (
  let f1 o = match o with Obj_some_eols ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_some_eols"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_some_eols ol o in
  let f2 o = Obj_some_eols o in
  List.map f2 ol)),67;(fun ol o -> (
  let f1 o = match o with Obj_seen_id ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_seen_id"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_seen_id ol o in
  let f2 o = Obj_seen_id o in
  List.map f2 ol)),66;(fun ol o -> (
  let f1 o = match o with Obj_scoped_id ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_scoped_id"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_scoped_id ol o in
  let f2 o = Obj_scoped_id o in
  List.map f2 ol)),65;(fun ol o -> (
  let f1 o = match o with Obj_scope_module ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_scope_module"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_scope_module ol o in
  let f2 o = Obj_scope_module o in
  List.map f2 ol)),64;(fun ol o -> (
  let f1 o = match o with Obj_scope_end ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_scope_end"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_scope_end ol o in
  let f2 o = Obj_scope_end o in
  List.map f2 ol)),63;(fun ol o -> (
  let f1 o = match o with Obj_scope_def ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_scope_def"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_scope_def ol o in
  let f2 o = Obj_scope_def o in
  List.map f2 ol)),62;(fun ol o -> (
  let f1 o = match o with Obj_scope_class ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_scope_class"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_scope_class ol o in
  let f2 o = Obj_scope_class o in
  List.map f2 ol)),61;(fun ol o -> (
  let f1 o = match o with Obj_scope_begin ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_scope_begin"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_scope_begin ol o in
  let f2 o = Obj_scope_begin o in
  List.map f2 ol)),60;(fun ol o -> (
  let f1 o = match o with Obj_rescue_list_rest ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_rescue_list_rest"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_rescue_list_rest ol o in
  let f2 o = Obj_rescue_list_rest o in
  List.map f2 ol)),59;(fun ol o -> (
  let f1 o = match o with Obj_rescue_list ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_rescue_list"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_rescue_list ol o in
  let f2 o = Obj_rescue_list o in
  List.map f2 ol)),58;(fun ol o -> (
  let f1 o = match o with Obj_rescue_clause ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_rescue_clause"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_rescue_clause ol o in
  let f2 o = Obj_rescue_clause o in
  List.map f2 ol)),57;(fun ol o -> (
  let f1 o = match o with Obj_primary ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_primary"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_primary ol o in
  let f2 o = Obj_primary o in
  List.map f2 ol)),56;(fun ol o -> (
  let f1 o = match o with Obj_one_string ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_one_string"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_one_string ol o in
  let f2 o = Obj_one_string o in
  List.map f2 ol)),55;(fun ol o -> (
  let f1 o = match o with Obj_mrhs ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_mrhs"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_mrhs ol o in
  let f2 o = Obj_mrhs o in
  List.map f2 ol)),54;(fun ol o -> (
  let f1 o = match o with Obj_mlhs_rest ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_mlhs_rest"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_mlhs_rest ol o in
  let f2 o = Obj_mlhs_rest o in
  List.map f2 ol)),53;(fun ol o -> (
  let f1 o = match o with Obj_mlhs_item ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_mlhs_item"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_mlhs_item ol o in
  let f2 o = Obj_mlhs_item o in
  List.map f2 ol)),52;(fun ol o -> (
  let f1 o = match o with Obj_mlhs_clean ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_mlhs_clean"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_mlhs_clean ol o in
  let f2 o = Obj_mlhs_clean o in
  List.map f2 ol)),51;(fun ol o -> (
  let f1 o = match o with Obj_mlhs_assign_op ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_mlhs_assign_op"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_mlhs_assign_op ol o in
  let f2 o = Obj_mlhs_assign_op o in
  List.map f2 ol)),50;(fun ol o -> (
  let f1 o = match o with Obj_mlhs ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_mlhs"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_mlhs ol o in
  let f2 o = Obj_mlhs o in
  List.map f2 ol)),49;(fun ol o -> (
  let f1 o = match o with Obj_method_name ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_method_name"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_method_name ol o in
  let f2 o = Obj_method_name o in
  List.map f2 ol)),48;(fun ol o -> (
  let f1 o = match o with Obj_method_formals ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_method_formals"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_method_formals ol o in
  let f2 o = Obj_method_formals o in
  List.map f2 ol)),47;(fun ol o -> (
  let f1 o = match o with Obj_meth_or_atom_list ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_meth_or_atom_list"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_meth_or_atom_list ol o in
  let f2 o = Obj_meth_or_atom_list o in
  List.map f2 ol)),46;(fun ol o -> (
  let f1 o = match o with Obj_meth_or_atom ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_meth_or_atom"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_meth_or_atom ol o in
  let f2 o = Obj_meth_or_atom o in
  List.map f2 ol)),45;(fun ol o -> (
  let f1 o = match o with Obj_message_identifier ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_message_identifier"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_message_identifier ol o in
  let f2 o = Obj_message_identifier o in
  List.map f2 ol)),44;(fun ol o -> (
  let f1 o = match o with Obj_main ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_main"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_main ol o in
  let f2 o = Obj_main o in
  List.map f2 ol)),43;(fun ol o -> (
  let f1 o = match o with Obj_lparen ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_lparen"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_lparen ol o in
  let f2 o = Obj_lparen o in
  List.map f2 ol)),42;(fun ol o -> (
  let f1 o = match o with Obj_lhs_pruned_assign_op ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_lhs_pruned_assign_op"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_lhs_pruned_assign_op ol o in
  let f2 o = Obj_lhs_pruned_assign_op o in
  List.map f2 ol)),41;(fun ol o -> (
  let f1 o = match o with Obj_lhs_prune_binop ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_lhs_prune_binop"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_lhs_prune_binop ol o in
  let f2 o = Obj_lhs_prune_binop o in
  List.map f2 ol)),40;(fun ol o -> (
  let f1 o = match o with Obj_lhs_assign_op ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_lhs_assign_op"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_lhs_assign_op ol o in
  let f2 o = Obj_lhs_assign_op o in
  List.map f2 ol)),39;(fun ol o -> (
  let f1 o = match o with Obj_lhs ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_lhs"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_lhs ol o in
  let f2 o = Obj_lhs o in
  List.map f2 ol)),38;(fun ol o -> (
  let f1 o = match o with Obj_keyword_as_id ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_keyword_as_id"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_keyword_as_id ol o in
  let f2 o = Obj_keyword_as_id o in
  List.map f2 ol)),37;(fun ol o -> (
  let f1 o = match o with Obj_interp_str_work ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_interp_str_work"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_interp_str_work ol o in
  let f2 o = Obj_interp_str_work o in
  List.map f2 ol)),36;(fun ol o -> (
  let f1 o = match o with Obj_interp_str ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_interp_str"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_interp_str ol o in
  let f2 o = Obj_interp_str o in
  List.map f2 ol)),35;(fun ol o -> (
  let f1 o = match o with Obj_interp_code ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_interp_code"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_interp_code ol o in
  let f2 o = Obj_interp_code o in
  List.map f2 ol)),34;(fun ol o -> (
  let f1 o = match o with Obj_if_else_clauses ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_if_else_clauses"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_if_else_clauses ol o in
  let f2 o = Obj_if_else_clauses o in
  List.map f2 ol)),33;(fun ol o -> (
  let f1 o = match o with Obj_identifier ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_identifier"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_identifier ol o in
  let f2 o = Obj_identifier o in
  List.map f2 ol)),32;(fun ol o -> (
  let f1 o = match o with Obj_func ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_func"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_func ol o in
  let f2 o = Obj_func o in
  List.map f2 ol)),31;(fun ol o -> (
  let f1 o = match o with Obj_formal_arg_nonempty_list ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_formal_arg_nonempty_list"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_formal_arg_nonempty_list ol o in
  let f2 o = Obj_formal_arg_nonempty_list o in
  List.map f2 ol)),30;(fun ol o -> (
  let f1 o = match o with Obj_formal_arg_list ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_formal_arg_list"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_formal_arg_list ol o in
  let f2 o = Obj_formal_arg_list o in
  List.map f2 ol)),29;(fun ol o -> (
  let f1 o = match o with Obj_formal_arg ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_formal_arg"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_formal_arg ol o in
  let f2 o = Obj_formal_arg o in
  List.map f2 ol)),28;(fun ol o -> (
  let f1 o = match o with Obj_expr ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_expr"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_expr ol o in
  let f2 o = Obj_expr o in
  List.map f2 ol)),27;(fun ol o -> (
  let f1 o = match o with Obj_eols ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_eols"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_eols ol o in
  let f2 o = Obj_eols o in
  List.map f2 ol)),26;(fun ol o -> (
  let f1 o = match o with Obj_eol_or_semi ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_eol_or_semi"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_eol_or_semi ol o in
  let f2 o = Obj_eol_or_semi o in
  List.map f2 ol)),25;(fun ol o -> (
  let f1 o = match o with Obj_ensure_clause ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_ensure_clause"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_ensure_clause ol o in
  let f2 o = Obj_ensure_clause o in
  List.map f2 ol)),24;(fun ol o -> (
  let f1 o = match o with Obj_do_sep ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_do_sep"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_do_sep ol o in
  let f2 o = Obj_do_sep o in
  List.map f2 ol)),23;(fun ol o -> (
  let f1 o = match o with Obj_do_codeblock ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_do_codeblock"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_do_codeblock ol o in
  let f2 o = Obj_do_codeblock o in
  List.map f2 ol)),22;(fun ol o -> (
  let f1 o = match o with Obj_constant ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_constant"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_constant ol o in
  let f2 o = Obj_constant o in
  List.map f2 ol)),21;(fun ol o -> (
  let f1 o = match o with Obj_command_name ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_command_name"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_command_name ol o in
  let f2 o = Obj_command_name o in
  List.map f2 ol)),20;(fun ol o -> (
  let f1 o = match o with Obj_command_codeblock ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_command_codeblock"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_command_codeblock ol o in
  let f2 o = Obj_command_codeblock o in
  List.map f2 ol)),19;(fun ol o -> (
  let f1 o = match o with Obj_command ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_command"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_command ol o in
  let f2 o = Obj_command o in
  List.map f2 ol)),18;(fun ol o -> (
  let f1 o = match o with Obj_code_block_body ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_code_block_body"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_code_block_body ol o in
  let f2 o = Obj_code_block_body o in
  List.map f2 ol)),17;(fun ol o -> (
  let f1 o = match o with Obj_code_block ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_code_block"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_code_block ol o in
  let f2 o = Obj_code_block o in
  List.map f2 ol)),16;(fun ol o -> (
  let f1 o = match o with Obj_class_inheritance ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_class_inheritance"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_class_inheritance ol o in
  let f2 o = Obj_class_inheritance o in
  List.map f2 ol)),15;(fun ol o -> (
  let f1 o = match o with Obj_case_else ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_case_else"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_case_else ol o in
  let f2 o = Obj_case_else o in
  List.map f2 ol)),14;(fun ol o -> (
  let f1 o = match o with Obj_call_args ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_call_args"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_call_args ol o in
  let f2 o = Obj_call_args o in
  List.map f2 ol)),13;(fun ol o -> (
  let f1 o = match o with Obj_brace_codeblock ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_brace_codeblock"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_brace_codeblock ol o in
  let f2 o = Obj_brace_codeblock o in
  List.map f2 ol)),12;(fun ol o -> (
  let f1 o = match o with Obj_body_exn ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_body_exn"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_body_exn ol o in
  let f2 o = Obj_body_exn o in
  List.map f2 ol)),11;(fun ol o -> (
  let f1 o = match o with Obj_bin_op ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_bin_op"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_bin_op ol o in
  let f2 o = Obj_bin_op o in
  List.map f2 ol)),10;(fun ol o -> (
  let f1 o = match o with Obj_assignable ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_assignable"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_assignable ol o in
  let f2 o = Obj_assignable o in
  List.map f2 ol)),9;(fun ol o -> (
  let f1 o = match o with Obj_assign_op ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_assign_op"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_assign_op ol o in
  let f2 o = Obj_assign_op o in
  List.map f2 ol)),8;(fun ol o -> (
  let f1 o = match o with Obj_array_item ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_array_item"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_array_item ol o in
  let f2 o = Obj_array_item o in
  List.map f2 ol)),7;(fun ol o -> (
  let f1 o = match o with Obj_array_body_rest ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_array_body_rest"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_array_body_rest ol o in
  let f2 o = Obj_array_body_rest o in
  List.map f2 ol)),6;(fun ol o -> (
  let f1 o = match o with Obj_array_body ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_array_body"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_array_body ol o in
  let f2 o = Obj_array_body o in
  List.map f2 ol)),5;(fun ol o -> (
  let f1 o = match o with Obj_arg_comma_star_list ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_arg_comma_star_list"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_arg_comma_star_list ol o in
  let f2 o = Obj_arg_comma_star_list o in
  List.map f2 ol)),4;(fun ol o -> (
  let f1 o = match o with Obj_arg_comma_nonempty_list ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_arg_comma_nonempty_list"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_arg_comma_nonempty_list ol o in
  let f2 o = Obj_arg_comma_nonempty_list o in
  List.map f2 ol)),3;(fun ol o -> (
  let f1 o = match o with Obj_arg_comma_list_trail ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_arg_comma_list_trail"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_arg_comma_list_trail ol o in
  let f2 o = Obj_arg_comma_list_trail o in
  List.map f2 ol)),2;(fun ol o -> (
  let f1 o = match o with Obj_arg ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_arg"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_arg ol o in
  let f2 o = Obj_arg o in
  List.map f2 ol)),1]

let __dypgen_automaton = Dyp_engine.create_parsing_device __dypgen_ra_list Dyp_priority_data.priority_data `LR0 !global_data !local_data __dypgen_merge_map dyp_merge Dyp_aux_functions.datadyn Dyp_symbols_array.str_non_ter Dyp_symbols_array.cons_of_nt

let __dypgen_data_equal = {
  Dyp_runtime.Tools.global_data_equal = global_data_equal;
  Dyp_runtime.Tools.local_data_equal = local_data_equal }

let main f lexbuf =
  let automaton = Dyp_engine.update_parsing_device_data __dypgen_automaton !global_data
    !local_data in
  let pf = Dyp_engine.glrParse automaton Dyp_aux_functions.get_token_value
    Dyp_symbols.get_token_name Dyp_symbols.str_token
    Dyp_symbols.main __dypgen_data_equal Dyp_symbols_array.test_cons Dyp_symbols_array.str_cons f lexbuf
    Dyp_aux_functions.lexbuf_position in
  let aux1 (o,p) = match o with
    | Obj_main r -> (r,p)
    | _ -> failwith "Wrong type for entry result" in
  List.map aux1 pf

