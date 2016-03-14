type token =
  | NUMBER of (float)
  | IDENT of (string)
  | BOOLEAN_TYPE
  | NUMBER_TYPE
  | STRING_TYPE
  | STREAM_TYPE
  | LIST_TYPE
  | FUNCTION_TYPE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | NOT
  | POWER_OF
  | OR
  | AND
  | FOR
  | FOREVER
  | IN
  | IF
  | THEN
  | ELSE
  | WHILE
  | SWITCH
  | BREAK
  | CONTINUE
  | RETURN
  | TRUE
  | FALSE
  | STDIN
  | END_OF_STATEMENT
  | EOF
  | LESS_THAN
  | LESS_THAN_EQUAL
  | GREATER_THAN
  | GREATER_THAN_EQUAL
  | EQUAL_TO
  | NOT_EQUAL_TO
  | SCOPE_BRACE_LEFT
  | SCOPE_BRACE_RIGHT
  | EQUALS
  | PLUS_EQUALS
  | MINUS_EQUALS
  | MULTIPLY_EQUALS
  | DIVIDE_EQUALS
  | SHOW
  | RANGE
  | SPLIT
  | SQUARE_BRACE_LEFT
  | SQUARE_BRACE_RIGHT
  | SEPARATOR
  | STRING_WRAPPER
  | ESCAPE_CHAR
  | LPAREN
  | RPAREN

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
    open Splat
# 64 "parser.ml"
let yytransl_const = [|
  259 (* BOOLEAN_TYPE *);
  260 (* NUMBER_TYPE *);
  261 (* STRING_TYPE *);
  262 (* STREAM_TYPE *);
  263 (* LIST_TYPE *);
  264 (* FUNCTION_TYPE *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* MODULO *);
  270 (* NOT *);
  271 (* POWER_OF *);
  272 (* OR *);
  273 (* AND *);
  274 (* FOR *);
  275 (* FOREVER *);
  276 (* IN *);
  277 (* IF *);
  278 (* THEN *);
  279 (* ELSE *);
  280 (* WHILE *);
  281 (* SWITCH *);
  282 (* BREAK *);
  283 (* CONTINUE *);
  284 (* RETURN *);
  285 (* TRUE *);
  286 (* FALSE *);
  287 (* STDIN *);
  288 (* END_OF_STATEMENT *);
    0 (* EOF *);
  289 (* LESS_THAN *);
  290 (* LESS_THAN_EQUAL *);
  291 (* GREATER_THAN *);
  292 (* GREATER_THAN_EQUAL *);
  293 (* EQUAL_TO *);
  294 (* NOT_EQUAL_TO *);
  295 (* SCOPE_BRACE_LEFT *);
  296 (* SCOPE_BRACE_RIGHT *);
  297 (* EQUALS *);
  298 (* PLUS_EQUALS *);
  299 (* MINUS_EQUALS *);
  300 (* MULTIPLY_EQUALS *);
  301 (* DIVIDE_EQUALS *);
  302 (* SHOW *);
  303 (* RANGE *);
  304 (* SPLIT *);
  305 (* SQUARE_BRACE_LEFT *);
  306 (* SQUARE_BRACE_RIGHT *);
  307 (* SEPARATOR *);
  308 (* STRING_WRAPPER *);
  309 (* ESCAPE_CHAR *);
  310 (* LPAREN *);
  311 (* RPAREN *);
    0|]

let yytransl_block = [|
  257 (* NUMBER *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\004\000\003\000\
\001\000\001\000\001\000\001\000\003\000\003\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\010\000\000\000\012\000\011\000\000\000\
\024\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\021\000\000\000\
\000\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\009\000\000\000\010\000"

let yysindex = "\255\255\
\002\255\000\000\000\000\000\000\002\255\000\000\000\000\002\255\
\000\000\072\000\000\000\008\255\002\255\002\255\002\255\002\255\
\002\255\002\255\002\255\000\000\002\255\002\255\000\000\251\254\
\251\254\000\000\247\254\247\254\017\255\048\255\055\255\055\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\042\000\000\000\001\000\010\000\005\000\012\000\038\000\045\000"

let yygindex = "\000\000\
\000\000\000\000\071\000"

let yytablesize = 363
let yytable = "\001\000\
\019\000\015\000\003\000\004\000\014\000\015\000\016\000\017\000\
\000\000\020\000\000\000\013\000\000\000\000\000\016\000\005\000\
\013\000\014\000\015\000\016\000\017\000\000\000\000\000\018\000\
\019\000\013\000\014\000\015\000\016\000\017\000\006\000\007\000\
\000\000\019\000\000\000\000\000\000\000\022\000\000\000\000\000\
\021\000\017\000\022\000\000\000\023\000\000\000\000\000\000\000\
\000\000\021\000\000\000\022\000\000\000\000\000\000\000\008\000\
\013\000\014\000\015\000\016\000\017\000\000\000\023\000\013\000\
\014\000\015\000\016\000\017\000\000\000\000\000\000\000\020\000\
\000\000\000\000\000\000\011\000\000\000\000\000\012\000\000\000\
\021\000\000\000\022\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\000\000\031\000\032\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\019\000\000\000\019\000\019\000\000\000\000\000\
\019\000\019\000\020\000\020\000\014\000\020\000\020\000\016\000\
\016\000\020\000\020\000\013\000\013\000\000\000\016\000\016\000\
\000\000\019\000\000\000\019\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\000\000\020\000\000\000\000\000\016\000\
\000\000\016\000\017\000\017\000\000\000\022\000\022\000\019\000\
\000\000\017\000\017\000\014\000\023\000\023\000\000\000\000\000\
\020\000\000\000\013\000\000\000\000\000\016\000\022\000\000\000\
\022\000\000\000\017\000\000\000\017\000\023\000\000\000\023\000\
\013\000\014\000\015\000\016\000\017\000\000\000\000\000\018\000\
\019\000\000\000\000\000\000\000\022\000\000\000\000\000\000\000\
\017\000\000\000\000\000\023\000\000\000\000\000\000\000\000\000\
\021\000\000\000\022\000"

let yycheck = "\001\000\
\000\000\011\001\001\001\002\001\000\000\011\001\012\001\013\001\
\255\255\000\000\255\255\000\000\255\255\255\255\000\000\014\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\016\001\
\017\001\009\001\010\001\011\001\012\001\013\001\029\001\030\001\
\255\255\017\001\255\255\255\255\255\255\000\000\255\255\255\255\
\033\001\000\000\035\001\255\255\000\000\255\255\255\255\255\255\
\255\255\033\001\255\255\035\001\255\255\255\255\255\255\054\001\
\009\001\010\001\011\001\012\001\013\001\255\255\055\001\009\001\
\010\001\011\001\012\001\013\001\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\005\000\255\255\255\255\008\000\255\255\
\033\001\255\255\035\001\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\255\255\021\000\022\000\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\009\001\010\001\255\255\012\001\013\001\255\255\255\255\
\016\001\017\001\009\001\010\001\016\001\012\001\013\001\009\001\
\010\001\016\001\017\001\016\001\017\001\255\255\016\001\017\001\
\255\255\033\001\255\255\035\001\255\255\255\255\255\255\255\255\
\255\255\255\255\033\001\255\255\035\001\255\255\255\255\033\001\
\255\255\035\001\009\001\010\001\255\255\016\001\017\001\055\001\
\255\255\016\001\017\001\055\001\016\001\017\001\255\255\255\255\
\055\001\255\255\055\001\255\255\255\255\055\001\033\001\255\255\
\035\001\255\255\033\001\255\255\035\001\033\001\255\255\035\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\016\001\
\017\001\255\255\255\255\255\255\055\001\255\255\255\255\255\255\
\055\001\255\255\255\255\055\001\255\255\255\255\255\255\255\255\
\033\001\255\255\035\001"

let yynames_const = "\
  BOOLEAN_TYPE\000\
  NUMBER_TYPE\000\
  STRING_TYPE\000\
  STREAM_TYPE\000\
  LIST_TYPE\000\
  FUNCTION_TYPE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MODULO\000\
  NOT\000\
  POWER_OF\000\
  OR\000\
  AND\000\
  FOR\000\
  FOREVER\000\
  IN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  SWITCH\000\
  BREAK\000\
  CONTINUE\000\
  RETURN\000\
  TRUE\000\
  FALSE\000\
  STDIN\000\
  END_OF_STATEMENT\000\
  EOF\000\
  LESS_THAN\000\
  LESS_THAN_EQUAL\000\
  GREATER_THAN\000\
  GREATER_THAN_EQUAL\000\
  EQUAL_TO\000\
  NOT_EQUAL_TO\000\
  SCOPE_BRACE_LEFT\000\
  SCOPE_BRACE_RIGHT\000\
  EQUALS\000\
  PLUS_EQUALS\000\
  MINUS_EQUALS\000\
  MULTIPLY_EQUALS\000\
  DIVIDE_EQUALS\000\
  SHOW\000\
  RANGE\000\
  SPLIT\000\
  SQUARE_BRACE_LEFT\000\
  SQUARE_BRACE_RIGHT\000\
  SEPARATOR\000\
  STRING_WRAPPER\000\
  ESCAPE_CHAR\000\
  LPAREN\000\
  RPAREN\000\
  "

let yynames_block = "\
  NUMBER\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
             ( _1 )
# 327 "parser.ml"
               : Splat.splTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                        ( SplatNumber )
# 333 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                        ( SplatBoolean )
# 339 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
                        ( SplatString )
# 345 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                        ( SplatList )
# 351 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                        ( SplatStream )
# 357 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Splat.splType) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Splat.splType) in
    Obj.repr(
# 59 "parser.mly"
                                                 ( SplatFunction (_2, _4) )
# 366 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Splat.splType) in
    Obj.repr(
# 60 "parser.mly"
                              ( _2 )
# 373 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 64 "parser.mly"
                                    ( SplNumber _1 )
# 380 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                                    ( SplVariable _1 )
# 387 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                    ( SplBoolean false )
# 393 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                    ( SplBoolean true )
# 399 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                    ( SplAnd (_1, _3) )
# 407 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                    ( SplOr (_1, _3) )
# 415 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                    ( SplNot _2 )
# 422 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                                    ( SplPlus (_1, _3) )
# 430 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                    ( SplMinus (_1, _3) )
# 438 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                                    ( SplTimes (_1, _3) )
# 446 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                                    ( SplDivide (_1, _3) )
# 454 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                                    ( SplModulo (_1, _3) )
# 462 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                                    ( _2 )
# 469 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                                    ( SplLt (_1, _3) )
# 477 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                                    ( SplGt (_1, _3) )
# 485 "parser.ml"
               : 'expr))
(* Entry parser_main *)
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
let parser_main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Splat.splTerm)
