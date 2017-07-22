type token =
  | T_EOF
  | K_DEF of (Lexing.position)
  | K_CLASS of (Lexing.position)
  | K_MODULE of (Lexing.position)
  | K_CAST of (Lexing.position)
  | K_OR of (Lexing.position)
  | K_SELF of (Lexing.position)
  | T_BEGIN_LINE of (Lexing.position)
  | T_NEWLINE of (Lexing.position)
  | T_SEMICOLON of (Lexing.position)
  | T_COLON of (Lexing.position)
  | T_DOUBLE_COLON of (Lexing.position)
  | T_DOT of (Lexing.position)
  | T_STAR of (Lexing.position)
  | T_QUESTION of (Lexing.position)
  | T_CARROT of (Lexing.position)
  | T_BANG of (Lexing.position)
  | T_RARROW of (Lexing.position)
  | T_LPAREN of (Lexing.position)
  | T_RPAREN of (Lexing.position)
  | T_LESS of (Lexing.position)
  | T_GREATER of (Lexing.position)
  | T_COMMA of (Lexing.position)
  | T_LBRACKET of (Lexing.position)
  | T_RBRACKET of (Lexing.position)
  | T_LBRACE of (Lexing.position)
  | T_RBRACE of (Lexing.position)
  | T_SUBTYPE of (Lexing.position)
  | T_INST_VAR of (Lexing.position * string)
  | T_CONST_ID of (Lexing.position * string)
  | T_TYPE_ID of (Lexing.position * string)
  | T_METHOD_NAME of (Lexing.position * string)

open Parsing;;
let _ = parse_error;;
# 2 "typeAnnotParser.mly"
  open Printf

  let merge_quantifiers tvars cons pos = 
    let rec work acc tvars cons = match tvars, cons with
      | tvars, [] -> 
          let rest = List.map (fun x -> x,None) tvars in
            List.rev_append rest acc
      | [], (tv,b)::_ -> 
          Log.fatal (Log.of_loc pos) 
            "The type variable %a can not be used as a lower bound at this point."
            Annotation.format_quant_var tv
      | tv::rest_tvars, _ -> 
          let bounds,rest_cons = 
            List.partition (fun (tv',bound) -> tv = tv') cons
          in
            match bounds with
              | [] -> work ((tv,None)::acc) rest_tvars rest_cons
              | [(_,t)] -> work ((tv,Some t)::acc) rest_tvars rest_cons
              | lst ->
                  let t = Annotation.Type_Union (List.map snd lst) in
                    work ((tv,Some t)::acc) rest_tvars rest_cons
    in
    let final = work [] tvars cons in
      List.rev final

  let pragma key = match key with
    | "FIXME" -> Annotation.Type_Fixme
    | _ -> failwith ("Uknown pragma keyword: "^key)

# 68 "typeAnnotParser.ml"
let yytransl_const = [|
  257 (* T_EOF *);
    0|]

let yytransl_block = [|
  258 (* K_DEF *);
  259 (* K_CLASS *);
  260 (* K_MODULE *);
  261 (* K_CAST *);
  262 (* K_OR *);
  263 (* K_SELF *);
  264 (* T_BEGIN_LINE *);
  265 (* T_NEWLINE *);
  266 (* T_SEMICOLON *);
  267 (* T_COLON *);
  268 (* T_DOUBLE_COLON *);
  269 (* T_DOT *);
  270 (* T_STAR *);
  271 (* T_QUESTION *);
  272 (* T_CARROT *);
  273 (* T_BANG *);
  274 (* T_RARROW *);
  275 (* T_LPAREN *);
  276 (* T_RPAREN *);
  277 (* T_LESS *);
  278 (* T_GREATER *);
  279 (* T_COMMA *);
  280 (* T_LBRACKET *);
  281 (* T_RBRACKET *);
  282 (* T_LBRACE *);
  283 (* T_RBRACE *);
  284 (* T_SUBTYPE *);
  285 (* T_INST_VAR *);
  286 (* T_CONST_ID *);
  287 (* T_TYPE_ID *);
  288 (* T_METHOD_NAME *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\005\000\005\000\006\000\006\000\006\000\006\000\006\000\
\006\000\009\000\009\000\011\000\011\000\012\000\012\000\013\000\
\013\000\014\000\014\000\003\000\003\000\008\000\008\000\015\000\
\015\000\016\000\007\000\007\000\017\000\017\000\017\000\010\000\
\010\000\010\000\004\000\004\000\019\000\019\000\020\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\022\000\023\000\023\000\023\000\023\000\021\000\021\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\004\000\007\000\003\000\001\000\001\000\001\000\003\000\003\000\
\003\000\005\000\004\000\000\000\003\000\001\000\003\000\000\000\
\002\000\004\000\001\000\005\000\003\000\000\000\002\000\001\000\
\003\000\003\000\001\000\003\000\001\000\001\000\002\000\001\000\
\002\000\003\000\001\000\003\000\001\000\003\000\003\000\001\000\
\001\000\001\000\001\000\002\000\002\000\003\000\004\000\002\000\
\004\000\003\000\001\000\001\000\003\000\003\000\000\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\065\000\001\000\
\000\000\000\000\002\000\000\000\000\000\003\000\000\000\037\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\040\000\
\038\000\000\000\000\000\048\000\000\000\050\000\013\000\012\000\
\000\000\000\000\000\000\004\000\000\000\000\000\000\000\005\000\
\006\000\041\000\053\000\052\000\000\000\039\000\056\000\000\000\
\000\000\000\000\000\000\000\000\000\000\064\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\000\000\054\000\
\000\000\042\000\000\000\000\000\044\000\009\000\000\000\000\000\
\011\000\000\000\000\000\000\000\016\000\015\000\017\000\000\000\
\025\000\000\000\000\000\057\000\023\000\058\000\062\000\061\000\
\055\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\031\000\000\000\000\000\028\000\046\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\021\000\019\000\000\000\
\033\000\034\000\018\000\010\000"

let yydgoto = "\002\000\
\007\000\010\000\013\000\048\000\051\000\034\000\083\000\091\000\
\081\000\027\000\101\000\049\000\066\000\039\000\105\000\106\000\
\028\000\029\000\077\000\030\000\052\000\053\000\054\000"

let yysindex = "\047\000\
\161\255\000\000\076\255\084\255\088\255\143\255\000\000\000\000\
\031\255\055\255\000\000\007\255\078\255\000\000\079\255\000\000\
\061\255\143\255\143\255\245\254\068\255\143\255\253\254\000\000\
\000\000\100\255\030\255\000\000\107\255\000\000\000\000\000\000\
\096\255\000\255\040\255\000\000\103\255\101\255\108\255\000\000\
\000\000\000\000\000\000\000\000\143\255\000\000\000\000\104\255\
\114\255\130\255\121\255\120\255\125\255\000\000\000\000\116\255\
\143\255\143\255\141\255\163\255\018\255\241\254\000\000\018\255\
\143\255\142\255\131\255\143\255\000\000\143\255\253\254\000\000\
\253\254\000\000\134\255\155\255\000\000\000\000\123\255\145\255\
\000\000\137\255\147\255\149\255\000\000\000\000\000\000\154\255\
\000\000\018\255\172\255\000\000\000\000\000\000\000\000\000\000\
\000\000\143\255\145\255\163\255\165\255\142\255\018\255\000\000\
\000\000\162\255\156\255\000\000\000\000\168\255\164\255\143\255\
\177\255\000\000\018\255\143\255\143\255\000\000\000\000\163\255\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\255\000\000\000\000\000\000\167\255\000\000\
\000\000\000\000\049\255\000\000\077\255\000\000\000\000\000\000\
\000\000\000\000\062\255\000\000\000\000\037\255\003\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\099\255\
\000\000\000\000\170\255\000\000\171\255\000\000\000\000\000\000\
\000\000\000\000\188\255\000\000\000\000\000\000\000\000\000\000\
\000\000\181\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\106\255\000\000\000\000\000\000\179\255\
\000\000\000\000\000\000\176\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\179\255\000\000\000\000\189\255\000\000\000\000\
\000\000\109\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\140\000\196\000\250\255\193\000\000\000\197\255\101\000\
\166\255\251\255\105\000\025\000\000\000\000\000\090\000\000\000\
\198\255\239\255\108\000\000\000\000\000\000\000\052\000"

let yytablesize = 206
let yytable = "\026\000\
\043\000\044\000\084\000\035\000\088\000\084\000\037\000\045\000\
\017\000\111\000\060\000\024\000\024\000\051\000\085\000\086\000\
\087\000\035\000\051\000\046\000\061\000\051\000\051\000\051\000\
\016\000\050\000\024\000\031\000\032\000\124\000\051\000\107\000\
\051\000\082\000\051\000\051\000\038\000\051\000\051\000\051\000\
\076\000\056\000\017\000\114\000\084\000\027\000\027\000\001\000\
\025\000\049\000\057\000\056\000\062\000\080\000\049\000\036\000\
\107\000\049\000\049\000\049\000\024\000\031\000\032\000\094\000\
\027\000\035\000\049\000\035\000\049\000\067\000\049\000\049\000\
\014\000\049\000\049\000\049\000\008\000\043\000\040\000\041\000\
\076\000\075\000\014\000\009\000\011\000\043\000\043\000\043\000\
\014\000\089\000\042\000\012\000\093\000\080\000\043\000\012\000\
\043\000\047\000\043\000\043\000\055\000\043\000\043\000\043\000\
\059\000\119\000\045\000\022\000\022\000\122\000\123\000\063\000\
\058\000\080\000\045\000\045\000\045\000\032\000\022\000\032\000\
\022\000\064\000\095\000\045\000\096\000\045\000\068\000\045\000\
\045\000\016\000\045\000\045\000\045\000\069\000\017\000\065\000\
\018\000\019\000\020\000\021\000\070\000\022\000\099\000\071\000\
\072\000\074\000\023\000\073\000\009\000\016\000\092\000\090\000\
\024\000\025\000\017\000\097\000\018\000\019\000\020\000\021\000\
\098\000\022\000\003\000\004\000\005\000\006\000\023\000\046\000\
\102\000\016\000\100\000\103\000\024\000\025\000\017\000\104\000\
\018\000\019\000\020\000\021\000\108\000\079\000\112\000\116\000\
\115\000\117\000\023\000\120\000\008\000\030\000\118\000\063\000\
\024\000\025\000\060\000\059\000\020\000\035\000\078\000\030\000\
\015\000\033\000\113\000\110\000\121\000\109\000"

let yycheck = "\006\000\
\018\000\019\000\061\000\009\000\064\000\064\000\000\001\019\001\
\012\001\100\000\011\001\009\001\010\001\001\001\030\001\031\001\
\032\001\023\000\006\001\031\001\021\001\009\001\010\001\011\001\
\007\001\029\001\030\001\031\001\032\001\120\000\018\001\090\000\
\020\001\016\001\022\001\023\001\030\001\025\001\026\001\027\001\
\058\000\012\001\012\001\103\000\103\000\009\001\010\001\001\000\
\031\001\001\001\021\001\012\001\013\001\060\000\006\001\001\001\
\115\000\009\001\010\001\011\001\030\001\031\001\032\001\070\000\
\028\001\071\000\018\001\073\000\020\001\045\000\022\001\023\001\
\011\001\025\001\026\001\027\001\001\001\001\001\001\001\001\001\
\098\000\057\000\021\001\008\001\001\001\009\001\010\001\011\001\
\001\001\065\000\030\001\008\001\068\000\100\000\018\001\008\001\
\020\001\030\001\022\001\023\001\001\001\025\001\026\001\027\001\
\009\001\112\000\001\001\009\001\010\001\116\000\117\000\009\001\
\006\001\120\000\009\001\010\001\011\001\009\001\020\001\011\001\
\022\001\021\001\071\000\018\001\073\000\020\001\023\001\022\001\
\023\001\007\001\025\001\026\001\027\001\020\001\012\001\028\001\
\014\001\015\001\016\001\017\001\011\001\019\001\020\001\023\001\
\025\001\030\001\024\001\023\001\008\001\007\001\020\001\010\001\
\030\001\031\001\012\001\022\001\014\001\015\001\016\001\017\001\
\006\001\019\001\002\001\003\001\004\001\005\001\024\001\031\001\
\022\001\007\001\026\001\023\001\030\001\031\001\012\001\022\001\
\014\001\015\001\016\001\017\001\009\001\019\001\018\001\028\001\
\023\001\018\001\024\001\011\001\001\001\009\001\027\001\025\001\
\030\001\031\001\025\001\025\001\018\001\022\001\059\000\011\001\
\005\000\009\000\102\000\099\000\115\000\098\000"

let yynames_const = "\
  T_EOF\000\
  "

let yynames_block = "\
  K_DEF\000\
  K_CLASS\000\
  K_MODULE\000\
  K_CAST\000\
  K_OR\000\
  K_SELF\000\
  T_BEGIN_LINE\000\
  T_NEWLINE\000\
  T_SEMICOLON\000\
  T_COLON\000\
  T_DOUBLE_COLON\000\
  T_DOT\000\
  T_STAR\000\
  T_QUESTION\000\
  T_CARROT\000\
  T_BANG\000\
  T_RARROW\000\
  T_LPAREN\000\
  T_RPAREN\000\
  T_LESS\000\
  T_GREATER\000\
  T_COMMA\000\
  T_LBRACKET\000\
  T_RBRACKET\000\
  T_LBRACE\000\
  T_RBRACE\000\
  T_SUBTYPE\000\
  T_INST_VAR\000\
  T_CONST_ID\000\
  T_TYPE_ID\000\
  T_METHOD_NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    Obj.repr(
# 63 "typeAnnotParser.mly"
                   ( None )
# 295 "typeAnnotParser.ml"
               : Annotation.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    Obj.repr(
# 64 "typeAnnotParser.mly"
                   ( None )
# 302 "typeAnnotParser.ml"
               : Annotation.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    Obj.repr(
# 65 "typeAnnotParser.mly"
                   ( None )
# 309 "typeAnnotParser.ml"
               : Annotation.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'method_annotation_list) in
    Obj.repr(
# 67 "typeAnnotParser.mly"
                                       ( Some (Annotation.MethodType _2) )
# 317 "typeAnnotParser.ml"
               : Annotation.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'class_annotation) in
    Obj.repr(
# 68 "typeAnnotParser.mly"
                                       ( Some (Annotation.ClassType _2) )
# 325 "typeAnnotParser.ml"
               : Annotation.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'class_annotation) in
    Obj.repr(
# 69 "typeAnnotParser.mly"
                                       ( Some (Annotation.ClassType _2) )
# 333 "typeAnnotParser.ml"
               : Annotation.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    Obj.repr(
# 70 "typeAnnotParser.mly"
                           (Some (Annotation.ExprType _2))
# 341 "typeAnnotParser.ml"
               : Annotation.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'method_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 74 "typeAnnotParser.mly"
      ( [_2] )
# 350 "typeAnnotParser.ml"
               : 'method_annotation_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'method_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'method_annotation_list) in
    Obj.repr(
# 76 "typeAnnotParser.mly"
      ( _2::_4 )
# 360 "typeAnnotParser.ml"
               : 'method_annotation_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'method_name) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'type_id_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Lexing.position) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'constraint_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'method_sig) in
    Obj.repr(
# 80 "typeAnnotParser.mly"
      ( _1, (merge_quantifiers _3 _5 _2), _7 )
# 373 "typeAnnotParser.ml"
               : 'method_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'method_name) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'method_sig) in
    Obj.repr(
# 82 "typeAnnotParser.mly"
      ( _1, [], _3 )
# 382 "typeAnnotParser.ml"
               : 'method_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 85 "typeAnnotParser.mly"
                  ( Annotation.TIdent_Relative (snd _1) )
# 389 "typeAnnotParser.ml"
               : 'method_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 86 "typeAnnotParser.mly"
              ( Annotation.TIdent_Relative (snd _1) )
# 396 "typeAnnotParser.ml"
               : 'method_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_ident) in
    Obj.repr(
# 87 "typeAnnotParser.mly"
               ( _1 )
# 403 "typeAnnotParser.ml"
               : 'method_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 88 "typeAnnotParser.mly"
                               ( Annotation.TIdent_Scoped (_1,snd _3) )
# 412 "typeAnnotParser.ml"
               : 'method_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 89 "typeAnnotParser.mly"
                                ( Annotation.TIdent_Scoped (_1,snd _3) )
# 421 "typeAnnotParser.ml"
               : 'method_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 90 "typeAnnotParser.mly"
                                   ( Annotation.TIdent_Scoped (_1,snd _3) )
# 430 "typeAnnotParser.ml"
               : 'method_name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 93 "typeAnnotParser.mly"
                                               ( Annotation.MethodSig([],_3,_5) )
# 441 "typeAnnotParser.ml"
               : 'method_sig))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 95 "typeAnnotParser.mly"
      ( match _1 with
          | Annotation.Type_Tuple lst -> Annotation.MethodSig(lst,_2,_4) 
          | t -> Annotation.MethodSig([t],_2,_4) 
      )
# 454 "typeAnnotParser.ml"
               : 'method_sig))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "typeAnnotParser.mly"
    ( None )
# 460 "typeAnnotParser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'method_sig) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 102 "typeAnnotParser.mly"
                                 (Some _2)
# 469 "typeAnnotParser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 105 "typeAnnotParser.mly"
              ( [_1] )
# 476 "typeAnnotParser.ml"
               : 'type_expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr_comma_list) in
    Obj.repr(
# 106 "typeAnnotParser.mly"
                                           (_1::_3)
# 485 "typeAnnotParser.ml"
               : 'type_expr_comma_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "typeAnnotParser.mly"
    ( [] )
# 491 "typeAnnotParser.ml"
               : 'declared_subtypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr_comma_list) in
    Obj.repr(
# 110 "typeAnnotParser.mly"
                                   ( _2 )
# 499 "typeAnnotParser.ml"
               : 'declared_subtypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Lexing.position * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_id_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 113 "typeAnnotParser.mly"
                                             ( snd _1, _3 )
# 509 "typeAnnotParser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 114 "typeAnnotParser.mly"
               ( snd _1, [] )
# 516 "typeAnnotParser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'class_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'declared_subtypes) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'constraint_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 118 "typeAnnotParser.mly"
      ( let pos = _1 in
        let name,vars = _2 in
        let subs = _3 in
        let cons = _4 in
        let qlist = merge_quantifiers vars cons pos in
          (name,qlist,subs))
# 532 "typeAnnotParser.ml"
               : 'class_annotation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 125 "typeAnnotParser.mly"
      ( Log.fatal (Log.of_loc _1) "parse error in annotation" )
# 540 "typeAnnotParser.ml"
               : 'class_annotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "typeAnnotParser.mly"
    ( [] )
# 546 "typeAnnotParser.ml"
               : 'constraint_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bounded_quantifier_list) in
    Obj.repr(
# 129 "typeAnnotParser.mly"
                                        ( _2 )
# 554 "typeAnnotParser.ml"
               : 'constraint_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bounded_quantifier) in
    Obj.repr(
# 132 "typeAnnotParser.mly"
                       ([_1])
# 561 "typeAnnotParser.ml"
               : 'bounded_quantifier_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bounded_quantifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bounded_quantifier_list) in
    Obj.repr(
# 133 "typeAnnotParser.mly"
                                                       (_1::_3)
# 570 "typeAnnotParser.ml"
               : 'bounded_quantifier_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_var) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 136 "typeAnnotParser.mly"
                                 ( _1, _3 )
# 579 "typeAnnotParser.ml"
               : 'bounded_quantifier))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_var) in
    Obj.repr(
# 139 "typeAnnotParser.mly"
             ( [_1] )
# 586 "typeAnnotParser.ml"
               : 'type_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_var) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_id_list) in
    Obj.repr(
# 140 "typeAnnotParser.mly"
                                  ( _1::_3 )
# 595 "typeAnnotParser.ml"
               : 'type_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 143 "typeAnnotParser.mly"
           ( Annotation.QSelf )
# 602 "typeAnnotParser.ml"
               : 'type_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 144 "typeAnnotParser.mly"
              ( Annotation.QVar (snd _1) )
# 609 "typeAnnotParser.ml"
               : 'type_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 145 "typeAnnotParser.mly"
                       ( Annotation.QParam (snd _2) )
# 617 "typeAnnotParser.ml"
               : 'type_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 148 "typeAnnotParser.mly"
               ( Annotation.TIdent_Relative (snd _1) )
# 624 "typeAnnotParser.ml"
               : 'type_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 149 "typeAnnotParser.mly"
                              ( Annotation.TIdent_Absolute (snd _2) )
# 632 "typeAnnotParser.ml"
               : 'type_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 150 "typeAnnotParser.mly"
                                         ( Annotation.TIdent_Scoped(_1, snd _3) )
# 641 "typeAnnotParser.ml"
               : 'type_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'single_type_expr) in
    Obj.repr(
# 153 "typeAnnotParser.mly"
                     ( _1 )
# 648 "typeAnnotParser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'single_type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_type_list) in
    Obj.repr(
# 154 "typeAnnotParser.mly"
                                       ( Annotation.Type_Union (_1::_3))
# 657 "typeAnnotParser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'single_type_expr) in
    Obj.repr(
# 157 "typeAnnotParser.mly"
                     ( [_1] )
# 664 "typeAnnotParser.ml"
               : 'or_type_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'single_type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_type_list) in
    Obj.repr(
# 158 "typeAnnotParser.mly"
                                       (_1::_3)
# 673 "typeAnnotParser.ml"
               : 'or_type_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 162 "typeAnnotParser.mly"
      ( match _2 with
          | [] -> assert false
          | [x] -> x
          | lst -> Annotation.Type_Tuple lst )
# 685 "typeAnnotParser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_var) in
    Obj.repr(
# 168 "typeAnnotParser.mly"
             ( Annotation.Type_Var _1 )
# 692 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_ident) in
    Obj.repr(
# 169 "typeAnnotParser.mly"
               ( Annotation.Type_Ident _1 )
# 699 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 170 "typeAnnotParser.mly"
          ( _1 )
# 706 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 171 "typeAnnotParser.mly"
               ( Annotation.Type_Dynamic )
# 713 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'single_type_expr) in
    Obj.repr(
# 172 "typeAnnotParser.mly"
                                ( Annotation.Type_Optional _2 )
# 721 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'single_type_expr) in
    Obj.repr(
# 173 "typeAnnotParser.mly"
                            ( Annotation.Type_Varargs _2 )
# 729 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_or_method_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 174 "typeAnnotParser.mly"
                                               ( Annotation.Type_Object _2 )
# 738 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr_comma_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 176 "typeAnnotParser.mly"
      ( Annotation.Type_App(_1, _3) )
# 748 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position * string) in
    Obj.repr(
# 177 "typeAnnotParser.mly"
                      ( pragma (snd _2) )
# 756 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr_comma_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Lexing.position) in
    Obj.repr(
# 179 "typeAnnotParser.mly"
      ( Annotation.Type_ParamList _3 )
# 766 "typeAnnotParser.ml"
               : 'single_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lexing.position * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 182 "typeAnnotParser.mly"
                                 ( (snd _1), _3 )
# 775 "typeAnnotParser.ml"
               : 'field_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field_type) in
    Obj.repr(
# 185 "typeAnnotParser.mly"
               ( [_1],[] )
# 782 "typeAnnotParser.ml"
               : 'field_or_method_nonempty_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'method_type) in
    Obj.repr(
# 186 "typeAnnotParser.mly"
                ( [],[_1] )
# 789 "typeAnnotParser.ml"
               : 'field_or_method_nonempty_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'field_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field_or_method_nonempty_list) in
    Obj.repr(
# 188 "typeAnnotParser.mly"
      ( let f,m = _3 in (_1::f),m )
# 798 "typeAnnotParser.ml"
               : 'field_or_method_nonempty_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'method_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field_or_method_nonempty_list) in
    Obj.repr(
# 190 "typeAnnotParser.mly"
      ( let f,m = _3 in f, (_1::m) )
# 807 "typeAnnotParser.ml"
               : 'field_or_method_nonempty_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 193 "typeAnnotParser.mly"
    ( [],[] )
# 813 "typeAnnotParser.ml"
               : 'field_or_method_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field_or_method_nonempty_list) in
    Obj.repr(
# 194 "typeAnnotParser.mly"
                                  (_1)
# 820 "typeAnnotParser.ml"
               : 'field_or_method_list))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Annotation.t option)
