# 1 "typeAnnotLexer.mll"
  
  open TypeAnnotParser

# 6 "typeAnnotLexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\220\255\002\000\091\000\194\000\028\001\118\001\227\255\
    \228\255\229\255\000\000\231\255\232\255\233\255\234\255\235\255\
    \236\255\237\255\034\000\239\255\068\000\242\255\176\001\245\255\
    \246\255\247\255\028\000\120\000\006\000\011\000\015\000\004\000\
    \011\000\254\255\119\000\012\000\253\255\020\000\031\000\041\000\
    \036\000\019\000\019\000\014\000\251\255\021\000\035\000\015\000\
    \249\255\049\000\016\000\250\255\053\000\065\000\075\000\083\000\
    \017\000\248\255\251\001\070\002\145\002\220\002\039\003\114\003\
    \240\255\238\255\219\255\221\255\224\255\204\003\038\004\128\004\
    \218\004\052\005\142\005\153\000\222\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\032\000\031\000\031\000\031\000\255\255\
    \255\255\255\255\025\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\014\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\003\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\003\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\012\000\012\000\012\000\012\000\012\000\011\000\
    \255\255\255\255\255\255\255\255\255\255\031\000\029\000\031\000\
    \031\000\030\000\032\000\255\255\255\255";
  Lexing.lex_default =
   "\255\255\000\000\075\000\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\255\255\034\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\034\000\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\075\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\029\000\007\000\000\000\000\000\000\000\000\000\028\000\
    \030\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
    \030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \029\000\021\000\002\000\027\000\255\255\026\000\028\000\032\000\
    \017\000\016\000\025\000\029\000\013\000\018\000\019\000\030\000\
    \033\000\036\000\031\000\044\000\048\000\051\000\057\000\000\000\
    \000\000\000\000\020\000\008\000\010\000\066\000\009\000\024\000\
    \022\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\015\000\028\000\014\000\023\000\004\000\
    \065\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\006\000\
    \004\000\004\000\004\000\005\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\012\000\067\000\011\000\064\000\039\000\
    \038\000\255\255\255\255\052\000\049\000\045\000\042\000\043\000\
    \046\000\037\000\041\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\040\000\047\000\050\000\
    \067\000\053\000\067\000\035\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\054\000\055\000\
    \056\000\000\000\074\000\076\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\067\000\000\000\000\000\000\000\000\000\
    \000\000\068\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\000\000\000\000\000\000\067\000\
    \001\000\067\000\255\255\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\000\000\000\000\000\000\
    \000\000\004\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\067\000\000\000\000\000\
    \000\000\000\000\000\000\068\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\000\000\000\000\
    \000\000\067\000\000\000\067\000\000\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\255\255\
    \255\255\000\000\000\000\004\000\000\000\004\000\004\000\004\000\
    \004\000\071\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\067\000\
    \000\000\255\255\000\000\000\000\000\000\068\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \000\000\000\000\000\000\067\000\000\000\067\000\000\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\000\000\000\000\000\000\000\000\004\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \070\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\058\000\058\000\058\000\058\000\058\000\059\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\000\000\000\000\000\000\000\000\058\000\
    \000\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\000\000\000\000\
    \000\000\000\000\058\000\000\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\060\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\000\000\000\000\000\000\000\000\058\000\000\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\061\000\058\000\058\000\000\000\000\000\000\000\000\000\
    \058\000\000\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\062\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\000\000\
    \000\000\000\000\000\000\058\000\000\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \058\000\058\000\058\000\058\000\063\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\000\000\000\000\000\000\000\000\058\000\000\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\000\000\000\000\000\000\
    \000\000\058\000\000\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\067\000\000\000\000\000\
    \000\000\000\000\000\000\068\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\000\000\000\000\
    \000\000\067\000\000\000\067\000\000\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\000\000\
    \000\000\000\000\000\000\069\000\000\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\067\000\
    \000\000\000\000\000\000\000\000\000\000\068\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \000\000\000\000\000\000\067\000\000\000\067\000\000\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\000\000\000\000\000\000\000\000\004\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\067\000\000\000\000\000\000\000\000\000\000\000\068\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\000\000\000\000\000\000\067\000\000\000\067\000\
    \000\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\000\000\000\000\000\000\000\000\004\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\072\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\067\000\000\000\000\000\000\000\000\000\
    \000\000\068\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\000\000\000\000\000\000\067\000\
    \000\000\067\000\000\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\000\000\000\000\000\000\
    \000\000\004\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \073\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\067\000\000\000\000\000\
    \000\000\000\000\000\000\068\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\000\000\000\000\
    \000\000\067\000\000\000\067\000\000\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\000\000\
    \000\000\000\000\000\000\004\000\000\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\067\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \000\000\000\000\000\000\067\000\000\000\067\000\000\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\000\000\000\000\000\000\000\000\074\000\000\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\028\000\
    \028\000\255\255\255\255\255\255\029\000\255\255\255\255\255\255\
    \030\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\002\000\000\000\028\000\031\000\
    \000\000\000\000\000\000\029\000\000\000\000\000\000\000\030\000\
    \032\000\035\000\030\000\043\000\047\000\050\000\056\000\255\255\
    \255\255\255\255\000\000\000\000\000\000\010\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\020\000\026\000\
    \026\000\034\000\027\000\037\000\038\000\040\000\041\000\042\000\
    \045\000\026\000\039\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\039\000\046\000\049\000\
    \003\000\052\000\003\000\027\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\053\000\054\000\
    \055\000\255\255\003\000\075\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\255\255\255\255\255\255\255\255\
    \255\255\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\255\255\255\255\255\255\004\000\
    \000\000\004\000\002\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\255\255\255\255\255\255\
    \255\255\004\000\255\255\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\005\000\255\255\255\255\
    \255\255\255\255\255\255\005\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\255\255\255\255\
    \255\255\005\000\255\255\005\000\255\255\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\034\000\
    \027\000\255\255\255\255\005\000\255\255\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
    \255\255\075\000\255\255\255\255\255\255\006\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \255\255\255\255\255\255\006\000\255\255\006\000\255\255\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\255\255\255\255\255\255\255\255\006\000\255\255\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\255\255\255\255\255\255\255\255\022\000\
    \255\255\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\255\255\255\255\
    \255\255\255\255\058\000\255\255\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\255\255\255\255\255\255\255\255\059\000\255\255\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\255\255\255\255\255\255\255\255\
    \060\000\255\255\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\255\255\
    \255\255\255\255\255\255\061\000\255\255\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\255\255\255\255\255\255\255\255\062\000\255\255\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\255\255\255\255\255\255\
    \255\255\063\000\255\255\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\069\000\255\255\255\255\
    \255\255\255\255\255\255\069\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\255\255\255\255\
    \255\255\069\000\255\255\069\000\255\255\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\255\255\
    \255\255\255\255\255\255\069\000\255\255\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\070\000\
    \255\255\255\255\255\255\255\255\255\255\070\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \255\255\255\255\255\255\070\000\255\255\070\000\255\255\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\255\255\255\255\255\255\255\255\070\000\255\255\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\071\000\255\255\255\255\255\255\255\255\255\255\071\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\255\255\255\255\255\255\071\000\255\255\071\000\
    \255\255\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\255\255\255\255\255\255\255\255\071\000\
    \255\255\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\072\000\255\255\255\255\255\255\255\255\
    \255\255\072\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\255\255\255\255\255\255\072\000\
    \255\255\072\000\255\255\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\255\255\255\255\255\255\
    \255\255\072\000\255\255\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\073\000\255\255\255\255\
    \255\255\255\255\255\255\073\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\255\255\255\255\
    \255\255\073\000\255\255\073\000\255\255\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\255\255\
    \255\255\255\255\255\255\073\000\255\255\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\074\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \255\255\255\255\255\255\074\000\255\255\074\000\255\255\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\255\255\255\255\255\255\255\255\074\000\255\255\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 26 "typeAnnotLexer.mll"
              ( token lexbuf )
# 484 "typeAnnotLexer.ml"

  | 1 ->
# 27 "typeAnnotLexer.mll"
                                        ( token lexbuf )
# 489 "typeAnnotLexer.ml"

  | 2 ->
# 28 "typeAnnotLexer.mll"
          ( T_BEGIN_LINE (lexbuf.Lexing.lex_curr_p) )
# 494 "typeAnnotLexer.ml"

  | 3 ->
# 29 "typeAnnotLexer.mll"
                                  ( token lexbuf )
# 499 "typeAnnotLexer.ml"

  | 4 ->
# 32 "typeAnnotLexer.mll"
             ( K_CAST (lexbuf.Lexing.lex_curr_p) )
# 504 "typeAnnotLexer.ml"

  | 5 ->
# 33 "typeAnnotLexer.mll"
            ( K_DEF (lexbuf.Lexing.lex_curr_p) )
# 509 "typeAnnotLexer.ml"

  | 6 ->
# 34 "typeAnnotLexer.mll"
              ( K_CLASS (lexbuf.Lexing.lex_curr_p) )
# 514 "typeAnnotLexer.ml"

  | 7 ->
# 35 "typeAnnotLexer.mll"
               ( K_MODULE (lexbuf.Lexing.lex_curr_p) )
# 519 "typeAnnotLexer.ml"

  | 8 ->
# 38 "typeAnnotLexer.mll"
        ( T_STAR(lexbuf.Lexing.lex_curr_p) )
# 524 "typeAnnotLexer.ml"

  | 9 ->
# 39 "typeAnnotLexer.mll"
        ( T_QUESTION(lexbuf.Lexing.lex_curr_p) )
# 529 "typeAnnotLexer.ml"

  | 10 ->
# 40 "typeAnnotLexer.mll"
        ( T_CARROT(lexbuf.Lexing.lex_curr_p) )
# 534 "typeAnnotLexer.ml"

  | 11 ->
# 42 "typeAnnotLexer.mll"
             (Log.fatal (Log.of_loc lexbuf.Lexing.lex_curr_p)
                "deprecated @@FIXME, use !FIXME")
# 540 "typeAnnotLexer.ml"

  | 12 ->
# 45 "typeAnnotLexer.mll"
                   (T_INST_VAR(lexbuf.Lexing.lex_curr_p,Lexing.lexeme lexbuf))
# 545 "typeAnnotLexer.ml"

  | 13 ->
# 46 "typeAnnotLexer.mll"
        ( T_BANG(lexbuf.Lexing.lex_curr_p) )
# 550 "typeAnnotLexer.ml"

  | 14 ->
# 47 "typeAnnotLexer.mll"
        ( T_COLON (lexbuf.Lexing.lex_curr_p) )
# 555 "typeAnnotLexer.ml"

  | 15 ->
# 48 "typeAnnotLexer.mll"
         ( T_DOUBLE_COLON (lexbuf.Lexing.lex_curr_p) )
# 560 "typeAnnotLexer.ml"

  | 16 ->
# 49 "typeAnnotLexer.mll"
        ( T_DOT (lexbuf.Lexing.lex_curr_p) )
# 565 "typeAnnotLexer.ml"

  | 17 ->
# 50 "typeAnnotLexer.mll"
         ( T_RARROW (lexbuf.Lexing.lex_curr_p) )
# 570 "typeAnnotLexer.ml"

  | 18 ->
# 51 "typeAnnotLexer.mll"
        ( T_LPAREN (lexbuf.Lexing.lex_curr_p) )
# 575 "typeAnnotLexer.ml"

  | 19 ->
# 52 "typeAnnotLexer.mll"
        ( T_RPAREN (lexbuf.Lexing.lex_curr_p) )
# 580 "typeAnnotLexer.ml"

  | 20 ->
# 53 "typeAnnotLexer.mll"
        ( T_LBRACKET (lexbuf.Lexing.lex_curr_p) )
# 585 "typeAnnotLexer.ml"

  | 21 ->
# 54 "typeAnnotLexer.mll"
        ( T_RBRACKET (lexbuf.Lexing.lex_curr_p) )
# 590 "typeAnnotLexer.ml"

  | 22 ->
# 55 "typeAnnotLexer.mll"
        ( T_COMMA (lexbuf.Lexing.lex_curr_p) )
# 595 "typeAnnotLexer.ml"

  | 23 ->
# 56 "typeAnnotLexer.mll"
        ( T_LBRACE (lexbuf.Lexing.lex_curr_p) )
# 600 "typeAnnotLexer.ml"

  | 24 ->
# 57 "typeAnnotLexer.mll"
        ( T_RBRACE (lexbuf.Lexing.lex_curr_p) )
# 605 "typeAnnotLexer.ml"

  | 25 ->
# 58 "typeAnnotLexer.mll"
        ( T_LESS (lexbuf.Lexing.lex_curr_p) )
# 610 "typeAnnotLexer.ml"

  | 26 ->
# 59 "typeAnnotLexer.mll"
        ( T_GREATER (lexbuf.Lexing.lex_curr_p) )
# 615 "typeAnnotLexer.ml"

  | 27 ->
# 60 "typeAnnotLexer.mll"
        ( T_SEMICOLON (lexbuf.Lexing.lex_curr_p))
# 620 "typeAnnotLexer.ml"

  | 28 ->
# 61 "typeAnnotLexer.mll"
         ( T_NEWLINE (lexbuf.Lexing.lex_curr_p) )
# 625 "typeAnnotLexer.ml"

  | 29 ->
# 64 "typeAnnotLexer.mll"
         ( K_OR (lexbuf.Lexing.lex_curr_p) )
# 630 "typeAnnotLexer.ml"

  | 30 ->
# 65 "typeAnnotLexer.mll"
           ( K_SELF (lexbuf.Lexing.lex_curr_p) )
# 635 "typeAnnotLexer.ml"

  | 31 ->
let
# 67 "typeAnnotLexer.mll"
                  t
# 641 "typeAnnotLexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 67 "typeAnnotLexer.mll"
                    ( T_TYPE_ID (lexbuf.Lexing.lex_curr_p,t) )
# 645 "typeAnnotLexer.ml"

  | 32 ->
let
# 68 "typeAnnotLexer.mll"
                   t
# 651 "typeAnnotLexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 68 "typeAnnotLexer.mll"
                     ( T_CONST_ID (lexbuf.Lexing.lex_curr_p,t) )
# 655 "typeAnnotLexer.ml"

  | 33 ->
let
# 69 "typeAnnotLexer.mll"
                    s
# 661 "typeAnnotLexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 69 "typeAnnotLexer.mll"
                           ( T_METHOD_NAME (lexbuf.Lexing.lex_curr_p, s) )
# 665 "typeAnnotLexer.ml"

  | 34 ->
let
# 70 "typeAnnotLexer.mll"
                    s
# 671 "typeAnnotLexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 70 "typeAnnotLexer.mll"
                      ( T_METHOD_NAME (lexbuf.Lexing.lex_curr_p, s) )
# 675 "typeAnnotLexer.ml"

  | 35 ->
# 72 "typeAnnotLexer.mll"
        ( T_EOF )
# 680 "typeAnnotLexer.ml"

  | 36 ->
# 74 "typeAnnotLexer.mll"
         ( T_SUBTYPE (lexbuf.Lexing.lex_curr_p) )
# 685 "typeAnnotLexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

