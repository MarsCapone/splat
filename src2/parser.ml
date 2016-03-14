type token =
  | NUMBER of (float)
  | IDENT of (string)
  | LAMBDA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | LPAREN
  | RPAREN
  | LET
  | IN
  | EQUALS
  | IF
  | THEN
  | ELSE
  | BOOLEAN_TYPE
  | NUMBER_TYPE
  | FUNCTION_TYPE
  | STRING_TYPE
  | STREAM_TYPE
  | LIST_TYPE
  | TRUE
  | FALSE
  | LESS_THAN
  | GREATER_THAN
  | COLON
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
    open Splat
# 35 "parser.ml"
let yytransl_const = [|
  259 (* LAMBDA *);
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
  263 (* DIVIDE *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* LET *);
  267 (* IN *);
  268 (* EQUALS *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* BOOLEAN_TYPE *);
  273 (* NUMBER_TYPE *);
  274 (* FUNCTION_TYPE *);
  275 (* STRING_TYPE *);
  276 (* STREAM_TYPE *);
  277 (* LIST_TYPE *);
  278 (* TRUE *);
  279 (* FALSE *);
  280 (* LESS_THAN *);
  281 (* GREATER_THAN *);
  282 (* COLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUMBER *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\003\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\006\000\009\000\000\000\008\000\007\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\012\000\
\010\000\011\000\000\000\000\000"

let yydgoto = "\002\000\
\008\000\000\000\009\000"

let yysindex = "\255\255\
\003\255\000\000\000\000\000\000\003\255\000\000\000\000\000\000\
\001\000\004\255\003\255\003\255\003\255\003\255\000\000\000\000\
\000\000\000\000\002\255\002\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\003\000"

let yygindex = "\000\000\
\000\000\000\000\005\000"

let yytablesize = 282
let yytable = "\001\000\
\015\000\013\000\014\000\003\000\004\000\011\000\012\000\011\000\
\012\000\010\000\005\000\000\000\016\000\000\000\000\000\017\000\
\018\000\019\000\020\000\000\000\000\000\000\000\000\000\000\000\
\006\000\007\000\000\000\013\000\014\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\012\000\000\000\000\000\
\000\000\000\000\013\000\014\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\014\000"

let yycheck = "\001\000\
\000\000\000\000\000\000\001\001\002\001\004\001\005\001\004\001\
\005\001\005\000\008\001\255\255\009\001\255\255\255\255\011\000\
\012\000\013\000\014\000\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\255\255\024\001\025\001\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\255\255\255\255\
\255\255\255\255\009\001\009\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\024\001\025\001"

let yynames_const = "\
  LAMBDA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  LPAREN\000\
  RPAREN\000\
  LET\000\
  IN\000\
  EQUALS\000\
  IF\000\
  THEN\000\
  ELSE\000\
  BOOLEAN_TYPE\000\
  NUMBER_TYPE\000\
  FUNCTION_TYPE\000\
  STRING_TYPE\000\
  STREAM_TYPE\000\
  LIST_TYPE\000\
  TRUE\000\
  FALSE\000\
  LESS_THAN\000\
  GREATER_THAN\000\
  COLON\000\
  EOF\000\
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
# 26 "parser.mly"
             ( _1 )
# 215 "parser.ml"
               : Splat.splTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                ( SplatNumber)
# 221 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
                       ( SplatBoolean )
# 227 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Splat.splType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Splat.splType) in
    Obj.repr(
# 31 "parser.mly"
                                        ( SplatFunction (_1,_3) )
# 235 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Splat.splType) in
    Obj.repr(
# 32 "parser.mly"
                              (_2)
# 242 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 35 "parser.mly"
                                ( SplNumber _1 )
# 249 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                                  ( SplBoolean false )
# 255 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                                  ( SplBoolean true )
# 261 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
                                  ( SplVariable _1 )
# 268 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                                  ( SplPlus (_1, _3) )
# 276 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                                  ( SplMinus (_1, _3) )
# 284 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                                  ( _2 )
# 291 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                                  ( SplLt (_1, _3) )
# 299 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                                  ( SplGt (_1, _3) )
# 307 "parser.ml"
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