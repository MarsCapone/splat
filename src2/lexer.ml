# 2 "lexer.mll"
 
open Parser        (* The type token is defined in parser.mli *)

# 6 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\229\255\015\000\231\255\233\255\234\255\235\255\236\255\
    \237\255\239\255\027\000\053\000\079\000\105\000\131\000\160\000\
    \186\000\212\000\238\000\015\000\027\000\059\001\255\255\069\001\
    \030\001\056\001\082\001\108\001\134\001\160\001\186\001\212\001\
    \238\001\008\002\034\002\060\002\086\002\112\002\138\002\164\002\
    \190\002\216\002\242\002\012\003\038\003\064\003\090\003\116\003\
    \142\003\168\003\194\003\220\003\246\003\016\004\042\004\068\004\
    \094\004\120\004\146\004\172\004\198\004\224\004\250\004\020\005\
    \046\005\072\005\098\005\124\005\150\005\176\005\230\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\255\255\001\000\017\000\255\255\001\000\
    \015\000\015\000\015\000\015\000\015\000\002\000\015\000\015\000\
    \015\000\015\000\003\000\015\000\015\000\015\000\015\000\015\000\
    \004\000\015\000\005\000\015\000\015\000\015\000\006\000\008\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\007\000\
    \015\000\015\000\014\000\010\000\009\000\015\000\015\000\015\000\
    \011\000\015\000\013\000\015\000\015\000\012\000\255\255";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\022\000\022\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\004\000\000\000\009\000\000\000\021\000\019\000\000\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\006\000\000\000\008\000\003\000\007\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\019\000\000\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
    \000\000\010\000\018\000\010\000\010\000\011\000\014\000\010\000\
    \010\000\013\000\010\000\010\000\015\000\010\000\017\000\010\000\
    \010\000\010\000\010\000\016\000\012\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\070\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\067\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\062\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\061\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\059\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\060\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\048\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \049\000\010\000\010\000\010\000\010\000\010\000\000\000\000\000\
    \001\000\010\000\010\000\010\000\010\000\043\000\010\000\010\000\
    \010\000\044\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\035\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\030\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\024\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\019\000\000\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\025\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\026\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\027\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\028\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\029\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\031\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\032\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\033\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\034\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\036\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\037\000\010\000\010\000\010\000\038\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\041\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \039\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\040\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\042\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \047\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\045\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\046\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\056\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\050\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\051\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \052\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\053\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\054\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \055\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\057\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\058\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\065\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\063\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\064\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\066\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\068\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\069\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\020\000\255\255\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\002\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\255\255\255\255\
    \000\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\021\000\255\255\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\255\255\255\255\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
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

let rec lexer_main lexbuf =
    __ocaml_lex_lexer_main_rec lexbuf 0
and __ocaml_lex_lexer_main_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 6 "lexer.mll"
                          ( lexer_main lexbuf )
# 491 "lexer.ml"

  | 1 ->
let
# 7 "lexer.mll"
                                          lsm
# 497 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 7 "lexer.mll"
                                              ( NUMBER(float_of_string lsm) )
# 501 "lexer.ml"

  | 2 ->
# 10 "lexer.mll"
                  ( BOOLEAN_TYPE )
# 506 "lexer.ml"

  | 3 ->
# 11 "lexer.mll"
                  ( NUMBER_TYPE )
# 511 "lexer.ml"

  | 4 ->
# 12 "lexer.mll"
                  ( STRING_TYPE )
# 516 "lexer.ml"

  | 5 ->
# 13 "lexer.mll"
                  ( STREAM_TYPE )
# 521 "lexer.ml"

  | 6 ->
# 14 "lexer.mll"
                  ( LIST_TYPE )
# 526 "lexer.ml"

  | 7 ->
# 15 "lexer.mll"
                  ( FUNCTION_TYPE )
# 531 "lexer.ml"

  | 8 ->
# 17 "lexer.mll"
               ( LET )
# 536 "lexer.ml"

  | 9 ->
# 18 "lexer.mll"
               ( IN )
# 541 "lexer.ml"

  | 10 ->
# 19 "lexer.mll"
               ( IF )
# 546 "lexer.ml"

  | 11 ->
# 20 "lexer.mll"
               ( THEN )
# 551 "lexer.ml"

  | 12 ->
# 21 "lexer.mll"
               ( ELSE )
# 556 "lexer.ml"

  | 13 ->
# 22 "lexer.mll"
               ( TRUE )
# 561 "lexer.ml"

  | 14 ->
# 23 "lexer.mll"
               ( FALSE )
# 566 "lexer.ml"

  | 15 ->
let
# 24 "lexer.mll"
                    lxm
# 572 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 24 "lexer.mll"
                        ( IDENT(lxm) )
# 576 "lexer.ml"

  | 16 ->
# 25 "lexer.mll"
               ( PLUS )
# 581 "lexer.ml"

  | 17 ->
# 26 "lexer.mll"
               ( MINUS )
# 586 "lexer.ml"

  | 18 ->
# 27 "lexer.mll"
               ( LESS_THAN )
# 591 "lexer.ml"

  | 19 ->
# 28 "lexer.mll"
               ( GREATER_THAN )
# 596 "lexer.ml"

  | 20 ->
# 29 "lexer.mll"
               ( COLON )
# 601 "lexer.ml"

  | 21 ->
# 30 "lexer.mll"
               ( LPAREN )
# 606 "lexer.ml"

  | 22 ->
# 31 "lexer.mll"
               ( RPAREN )
# 611 "lexer.ml"

  | 23 ->
# 32 "lexer.mll"
               ( COLON )
# 616 "lexer.ml"

  | 24 ->
# 33 "lexer.mll"
               ( EQUALS )
# 621 "lexer.ml"

  | 25 ->
# 34 "lexer.mll"
                ( LAMBDA )
# 626 "lexer.ml"

  | 26 ->
# 35 "lexer.mll"
               ( EOF )
# 631 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_lexer_main_rec lexbuf __ocaml_lex_state

;;
