type token =
  | NUMBER of (float)
  | IDENT of (string)
  | STRING of (string)
  | BOOLEAN_TYPE
  | NUMBER_TYPE
  | STRING_TYPE
  | STREAM_TYPE
  | LIST_TYPE
  | VOID_TYPE
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
  | JUSTDO
  | IF
  | THEN
  | ELSE
  | CONS
  | HEAD
  | TAIL
  | EMPTY_LIST
  | EMPTY_STREAM
  | AS_NUM
  | APPLY
  | TRUE
  | FALSE
  | STDIN
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
  | SHOWLN
  | RANGE
  | SPLIT
  | SQUARE_BRACE_LEFT
  | SQUARE_BRACE_RIGHT
  | LET
  | SEPARATOR
  | STRING_WRAPPER
  | ESCAPE_CHAR
  | LPAREN
  | RPAREN
  | COMMENT_LEFT
  | COMMENT_RIGHT

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
    open Splat
    open Pervasives
# 70 "parser.ml"
let yytransl_const = [|
  260 (* BOOLEAN_TYPE *);
  261 (* NUMBER_TYPE *);
  262 (* STRING_TYPE *);
  263 (* STREAM_TYPE *);
  264 (* LIST_TYPE *);
  265 (* VOID_TYPE *);
  266 (* FUNCTION_TYPE *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIVIDE *);
  271 (* MODULO *);
  272 (* NOT *);
  273 (* POWER_OF *);
  274 (* OR *);
  275 (* AND *);
  276 (* JUSTDO *);
  277 (* IF *);
  278 (* THEN *);
  279 (* ELSE *);
  280 (* CONS *);
  281 (* HEAD *);
  282 (* TAIL *);
  283 (* EMPTY_LIST *);
  284 (* EMPTY_STREAM *);
  285 (* AS_NUM *);
  286 (* APPLY *);
  287 (* TRUE *);
  288 (* FALSE *);
  289 (* STDIN *);
    0 (* EOF *);
  290 (* LESS_THAN *);
  291 (* LESS_THAN_EQUAL *);
  292 (* GREATER_THAN *);
  293 (* GREATER_THAN_EQUAL *);
  294 (* EQUAL_TO *);
  295 (* NOT_EQUAL_TO *);
  296 (* SCOPE_BRACE_LEFT *);
  297 (* SCOPE_BRACE_RIGHT *);
  298 (* EQUALS *);
  299 (* PLUS_EQUALS *);
  300 (* MINUS_EQUALS *);
  301 (* MULTIPLY_EQUALS *);
  302 (* DIVIDE_EQUALS *);
  303 (* SHOW *);
  304 (* SHOWLN *);
  305 (* RANGE *);
  306 (* SPLIT *);
  307 (* SQUARE_BRACE_LEFT *);
  308 (* SQUARE_BRACE_RIGHT *);
  309 (* LET *);
  310 (* SEPARATOR *);
  311 (* STRING_WRAPPER *);
  312 (* ESCAPE_CHAR *);
  313 (* LPAREN *);
  314 (* RPAREN *);
  315 (* COMMENT_LEFT *);
  316 (* COMMENT_RIGHT *);
    0|]

let yytransl_block = [|
  257 (* NUMBER *);
  258 (* IDENT *);
  259 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\003\000\003\000\009\000\
\001\000\001\000\003\000\003\000\003\000\010\000\003\000\001\000\
\001\000\003\000\003\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\009\000\003\000\001\000\002\000\002\000\001\000\002\000\002\000\
\002\000\002\000\007\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\000\000\017\000\016\000\038\000\000\000\
\000\000\000\000\000\000\000\000\000\000\046\000\000\000\003\000\
\002\000\004\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\000\012\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\007\000\
\000\000\000\000\000\000\000\000\000\000\000\000\044\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\008\000\
\014\000"

let yydgoto = "\002\000\
\022\000\030\000\064\000\065\000"

let yysindex = "\009\000\
\087\255\000\000\000\000\000\000\017\255\087\255\230\254\087\255\
\087\255\087\255\000\000\087\255\000\000\000\000\000\000\087\255\
\087\255\087\255\223\254\010\255\087\255\000\000\189\001\000\000\
\000\000\000\000\000\000\017\255\017\255\022\255\002\255\087\255\
\117\002\161\002\161\002\161\002\161\002\147\002\147\002\024\255\
\229\254\235\254\134\255\087\255\087\255\087\255\087\255\087\255\
\087\255\087\255\087\255\087\255\087\255\000\000\087\255\087\255\
\087\255\087\255\087\255\087\255\017\255\233\254\236\254\164\255\
\253\254\087\255\254\254\000\000\000\000\000\000\016\255\016\255\
\002\255\245\254\245\254\002\255\199\002\228\002\147\002\190\002\
\055\255\055\255\055\255\055\255\064\255\064\255\000\000\000\000\
\017\255\087\255\255\254\218\001\087\255\039\255\000\000\087\255\
\019\255\196\255\251\254\249\001\020\255\021\255\031\255\000\000\
\087\255\087\255\087\255\024\002\055\002\086\002\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\130\000\161\000\111\001\138\001\119\001\141\001\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\255\
\000\000\000\000\000\000\000\000\000\000\000\000\156\000\187\000\
\032\000\094\000\125\000\063\000\096\001\089\001\157\001\116\001\
\212\000\237\000\006\001\032\001\057\001\064\001\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\231\255\255\255\228\255"

let yytablesize = 1035
let yytable = "\023\000\
\020\000\046\000\061\000\062\000\031\000\049\000\033\000\034\000\
\035\000\001\000\036\000\041\000\042\000\032\000\037\000\038\000\
\039\000\040\000\049\000\043\000\024\000\025\000\026\000\063\000\
\027\000\067\000\028\000\068\000\046\000\047\000\048\000\023\000\
\049\000\069\000\088\000\087\000\089\000\091\000\096\000\093\000\
\099\000\101\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\103\000\081\000\082\000\083\000\
\084\000\085\000\086\000\105\000\106\000\095\000\026\000\094\000\
\092\000\044\000\045\000\046\000\047\000\048\000\107\000\049\000\
\045\000\029\000\044\000\045\000\046\000\047\000\048\000\000\000\
\049\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\004\000\000\000\000\000\098\000\000\000\024\000\100\000\000\000\
\005\000\055\000\056\000\057\000\058\000\000\000\006\000\108\000\
\109\000\110\000\007\000\008\000\000\000\000\000\000\000\009\000\
\010\000\011\000\000\000\012\000\000\000\013\000\014\000\015\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\016\000\017\000\000\000\
\018\000\000\000\000\000\019\000\000\000\020\000\000\000\021\000\
\044\000\045\000\046\000\047\000\048\000\000\000\049\000\050\000\
\051\000\000\000\000\000\021\000\000\000\052\000\000\000\000\000\
\037\000\000\000\000\000\053\000\000\000\000\000\000\000\055\000\
\056\000\057\000\058\000\059\000\060\000\000\000\044\000\045\000\
\046\000\047\000\048\000\000\000\049\000\050\000\051\000\000\000\
\000\000\000\000\022\000\052\000\000\000\000\000\000\000\070\000\
\000\000\053\000\000\000\000\000\000\000\055\000\056\000\057\000\
\058\000\059\000\060\000\000\000\000\000\000\000\044\000\045\000\
\046\000\047\000\048\000\029\000\049\000\050\000\051\000\000\000\
\000\000\090\000\000\000\052\000\000\000\000\000\000\000\000\000\
\000\000\053\000\000\000\000\000\000\000\055\000\056\000\057\000\
\058\000\059\000\060\000\000\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\102\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\000\000\000\000\000\000\020\000\020\000\020\000\020\000\020\000\
\000\000\000\000\020\000\020\000\000\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\020\000\028\000\
\000\000\000\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\023\000\023\000\023\000\023\000\023\000\000\000\
\000\000\023\000\023\000\000\000\020\000\000\000\020\000\023\000\
\032\000\000\000\020\000\000\000\000\000\023\000\000\000\031\000\
\000\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\026\000\026\000\026\000\026\000\026\000\000\000\000\000\
\026\000\026\000\000\000\023\000\000\000\023\000\026\000\000\000\
\018\000\023\000\000\000\000\000\026\000\000\000\000\000\019\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\024\000\024\000\000\000\024\000\024\000\000\000\040\000\024\000\
\024\000\000\000\026\000\013\000\026\000\024\000\041\000\000\000\
\026\000\000\000\000\000\024\000\000\000\000\000\000\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\025\000\
\025\000\042\000\025\000\025\000\039\000\000\000\025\000\025\000\
\000\000\024\000\000\000\024\000\025\000\000\000\000\000\024\000\
\000\000\036\000\025\000\000\000\034\000\000\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\021\000\021\000\
\000\000\036\000\036\000\000\000\000\000\021\000\021\000\000\000\
\025\000\000\000\025\000\021\000\000\000\036\000\025\000\036\000\
\037\000\021\000\000\000\036\000\054\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\022\000\022\000\000\000\
\037\000\037\000\000\000\000\000\022\000\022\000\000\000\021\000\
\000\000\021\000\022\000\000\000\037\000\021\000\037\000\000\000\
\022\000\000\000\037\000\000\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\000\000\029\000\029\000\000\000\
\000\000\000\000\000\000\029\000\000\000\000\000\022\000\000\000\
\022\000\029\000\000\000\000\000\022\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\000\000\027\000\027\000\
\000\000\000\000\000\000\000\000\027\000\000\000\000\000\029\000\
\000\000\029\000\027\000\000\000\000\000\029\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\000\000\030\000\
\030\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\027\000\000\000\027\000\030\000\000\000\000\000\027\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\000\000\
\000\000\028\000\028\000\000\000\000\000\000\000\000\000\028\000\
\000\000\030\000\000\000\030\000\000\000\028\000\000\000\030\000\
\000\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\000\000\032\000\032\000\000\000\000\000\000\000\000\000\
\032\000\031\000\031\000\028\000\000\000\028\000\032\000\031\000\
\000\000\028\000\000\000\000\000\000\000\031\000\032\000\032\000\
\032\000\032\000\000\000\000\000\000\000\031\000\031\000\031\000\
\031\000\000\000\018\000\018\000\032\000\000\000\032\000\000\000\
\018\000\019\000\032\000\031\000\000\000\031\000\018\000\019\000\
\000\000\031\000\000\000\000\000\000\000\019\000\000\000\000\000\
\018\000\018\000\000\000\000\000\000\000\000\000\040\000\019\000\
\019\000\000\000\000\000\013\000\018\000\000\000\018\000\000\000\
\000\000\013\000\018\000\019\000\000\000\019\000\040\000\040\000\
\000\000\019\000\000\000\013\000\013\000\000\000\041\000\041\000\
\000\000\042\000\040\000\000\000\040\000\000\000\000\000\013\000\
\040\000\013\000\041\000\000\000\041\000\013\000\000\000\000\000\
\041\000\042\000\042\000\000\000\039\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\042\000\000\000\042\000\
\039\000\000\000\039\000\042\000\034\000\034\000\039\000\044\000\
\045\000\046\000\047\000\048\000\000\000\049\000\050\000\051\000\
\034\000\000\000\034\000\000\000\052\000\000\000\034\000\000\000\
\000\000\000\000\053\000\000\000\000\000\000\000\055\000\056\000\
\057\000\058\000\059\000\060\000\044\000\045\000\046\000\047\000\
\048\000\000\000\049\000\050\000\051\000\000\000\000\000\000\000\
\000\000\052\000\000\000\000\000\000\000\000\000\000\000\053\000\
\000\000\000\000\000\000\055\000\056\000\057\000\058\000\059\000\
\060\000\000\000\097\000\044\000\045\000\046\000\047\000\048\000\
\000\000\049\000\050\000\051\000\000\000\000\000\000\000\000\000\
\052\000\000\000\000\000\000\000\000\000\000\000\053\000\000\000\
\000\000\000\000\055\000\056\000\057\000\058\000\059\000\060\000\
\000\000\104\000\044\000\045\000\046\000\047\000\048\000\000\000\
\049\000\050\000\051\000\000\000\000\000\000\000\000\000\052\000\
\000\000\000\000\000\000\000\000\000\000\053\000\000\000\000\000\
\000\000\055\000\056\000\057\000\058\000\059\000\060\000\000\000\
\111\000\044\000\045\000\046\000\047\000\048\000\000\000\049\000\
\050\000\051\000\000\000\000\000\000\000\000\000\052\000\000\000\
\000\000\000\000\000\000\000\000\053\000\000\000\000\000\000\000\
\055\000\056\000\057\000\058\000\059\000\060\000\000\000\112\000\
\044\000\045\000\046\000\047\000\048\000\000\000\049\000\050\000\
\051\000\000\000\000\000\000\000\000\000\052\000\000\000\000\000\
\000\000\000\000\000\000\053\000\000\000\000\000\000\000\055\000\
\056\000\057\000\058\000\059\000\060\000\000\000\113\000\044\000\
\045\000\046\000\047\000\048\000\000\000\049\000\050\000\051\000\
\000\000\000\000\000\000\000\000\052\000\000\000\000\000\000\000\
\000\000\000\000\053\000\000\000\000\000\000\000\055\000\056\000\
\057\000\058\000\059\000\060\000\066\000\044\000\045\000\046\000\
\047\000\048\000\000\000\049\000\050\000\051\000\000\000\000\000\
\000\000\000\000\052\000\044\000\045\000\046\000\047\000\048\000\
\053\000\049\000\050\000\051\000\055\000\056\000\057\000\058\000\
\059\000\060\000\000\000\000\000\000\000\000\000\053\000\000\000\
\000\000\000\000\055\000\056\000\057\000\058\000\059\000\060\000\
\044\000\045\000\046\000\047\000\048\000\000\000\049\000\050\000\
\051\000\044\000\045\000\046\000\047\000\048\000\000\000\049\000\
\000\000\051\000\000\000\000\000\000\000\000\000\000\000\055\000\
\056\000\057\000\058\000\059\000\060\000\000\000\000\000\000\000\
\055\000\056\000\057\000\058\000\059\000\060\000\044\000\045\000\
\046\000\047\000\048\000\000\000\049\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\055\000\056\000\057\000\
\058\000\059\000\060\000"

let yycheck = "\001\000\
\000\000\013\001\028\000\029\000\006\000\017\001\008\000\009\000\
\010\000\001\000\012\000\002\001\003\001\040\001\016\000\017\000\
\018\000\051\001\017\001\021\000\004\001\005\001\006\001\002\001\
\008\001\002\001\010\001\055\001\013\001\014\001\015\001\000\000\
\017\001\055\001\058\001\061\000\057\001\041\001\040\001\042\001\
\002\001\023\001\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\058\001\055\000\056\000\057\000\
\058\000\059\000\060\000\040\001\040\001\090\000\000\000\089\000\
\066\000\011\001\012\001\013\001\014\001\015\001\040\001\017\001\
\041\001\057\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\255\255\255\255\255\255\255\255\255\255\255\255\001\001\
\002\001\255\255\255\255\093\000\255\255\000\000\096\000\255\255\
\010\001\034\001\035\001\036\001\037\001\255\255\016\001\105\000\
\106\000\107\000\020\001\021\001\255\255\255\255\255\255\025\001\
\026\001\027\001\255\255\029\001\255\255\031\001\032\001\033\001\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\047\001\048\001\255\255\
\050\001\255\255\255\255\053\001\255\255\055\001\255\255\057\001\
\011\001\012\001\013\001\014\001\015\001\255\255\017\001\018\001\
\019\001\255\255\255\255\000\000\255\255\024\001\255\255\255\255\
\000\000\255\255\255\255\030\001\255\255\255\255\255\255\034\001\
\035\001\036\001\037\001\038\001\039\001\255\255\011\001\012\001\
\013\001\014\001\015\001\255\255\017\001\018\001\019\001\255\255\
\255\255\255\255\000\000\024\001\255\255\255\255\255\255\058\001\
\255\255\030\001\255\255\255\255\255\255\034\001\035\001\036\001\
\037\001\038\001\039\001\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\000\000\017\001\018\001\019\001\255\255\
\255\255\054\001\255\255\024\001\255\255\255\255\255\255\255\255\
\255\255\030\001\255\255\255\255\255\255\034\001\035\001\036\001\
\037\001\038\001\039\001\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\052\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\018\001\019\001\255\255\255\255\255\255\255\255\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001\000\000\
\255\255\255\255\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\041\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\018\001\019\001\255\255\052\001\255\255\054\001\024\001\
\000\000\255\255\058\001\255\255\255\255\030\001\255\255\000\000\
\255\255\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\041\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\018\001\019\001\255\255\052\001\255\255\054\001\024\001\255\255\
\000\000\058\001\255\255\255\255\030\001\255\255\255\255\000\000\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\041\001\
\011\001\012\001\255\255\014\001\015\001\255\255\000\000\018\001\
\019\001\255\255\052\001\000\000\054\001\024\001\000\000\255\255\
\058\001\255\255\255\255\030\001\255\255\255\255\255\255\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\011\001\
\012\001\000\000\014\001\015\001\000\000\255\255\018\001\019\001\
\255\255\052\001\255\255\054\001\024\001\255\255\255\255\058\001\
\255\255\024\001\030\001\255\255\000\000\255\255\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001\041\001\011\001\012\001\
\255\255\040\001\041\001\255\255\255\255\018\001\019\001\255\255\
\052\001\255\255\054\001\024\001\255\255\052\001\058\001\054\001\
\024\001\030\001\255\255\058\001\000\000\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\041\001\011\001\012\001\255\255\
\040\001\041\001\255\255\255\255\018\001\019\001\255\255\052\001\
\255\255\054\001\024\001\255\255\052\001\058\001\054\001\255\255\
\030\001\255\255\058\001\255\255\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001\255\255\018\001\019\001\255\255\
\255\255\255\255\255\255\024\001\255\255\255\255\052\001\255\255\
\054\001\030\001\255\255\255\255\058\001\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\041\001\255\255\018\001\019\001\
\255\255\255\255\255\255\255\255\024\001\255\255\255\255\052\001\
\255\255\054\001\030\001\255\255\255\255\058\001\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001\041\001\255\255\018\001\
\019\001\255\255\255\255\255\255\255\255\024\001\255\255\255\255\
\052\001\255\255\054\001\030\001\255\255\255\255\058\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\255\255\
\255\255\018\001\019\001\255\255\255\255\255\255\255\255\024\001\
\255\255\052\001\255\255\054\001\255\255\030\001\255\255\058\001\
\255\255\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\041\001\255\255\018\001\019\001\255\255\255\255\255\255\255\255\
\024\001\018\001\019\001\052\001\255\255\054\001\030\001\024\001\
\255\255\058\001\255\255\255\255\255\255\030\001\038\001\039\001\
\040\001\041\001\255\255\255\255\255\255\038\001\039\001\040\001\
\041\001\255\255\018\001\019\001\052\001\255\255\054\001\255\255\
\024\001\018\001\058\001\052\001\255\255\054\001\030\001\024\001\
\255\255\058\001\255\255\255\255\255\255\030\001\255\255\255\255\
\040\001\041\001\255\255\255\255\255\255\255\255\024\001\040\001\
\041\001\255\255\255\255\024\001\052\001\255\255\054\001\255\255\
\255\255\030\001\058\001\052\001\255\255\054\001\040\001\041\001\
\255\255\058\001\255\255\040\001\041\001\255\255\040\001\041\001\
\255\255\024\001\052\001\255\255\054\001\255\255\255\255\052\001\
\058\001\054\001\052\001\255\255\054\001\058\001\255\255\255\255\
\058\001\040\001\041\001\255\255\040\001\041\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\052\001\255\255\054\001\
\052\001\255\255\054\001\058\001\040\001\041\001\058\001\011\001\
\012\001\013\001\014\001\015\001\255\255\017\001\018\001\019\001\
\052\001\255\255\054\001\255\255\024\001\255\255\058\001\255\255\
\255\255\255\255\030\001\255\255\255\255\255\255\034\001\035\001\
\036\001\037\001\038\001\039\001\011\001\012\001\013\001\014\001\
\015\001\255\255\017\001\018\001\019\001\255\255\255\255\255\255\
\255\255\024\001\255\255\255\255\255\255\255\255\255\255\030\001\
\255\255\255\255\255\255\034\001\035\001\036\001\037\001\038\001\
\039\001\255\255\041\001\011\001\012\001\013\001\014\001\015\001\
\255\255\017\001\018\001\019\001\255\255\255\255\255\255\255\255\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001\255\255\
\255\255\255\255\034\001\035\001\036\001\037\001\038\001\039\001\
\255\255\041\001\011\001\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\255\255\255\255\255\255\255\255\024\001\
\255\255\255\255\255\255\255\255\255\255\030\001\255\255\255\255\
\255\255\034\001\035\001\036\001\037\001\038\001\039\001\255\255\
\041\001\011\001\012\001\013\001\014\001\015\001\255\255\017\001\
\018\001\019\001\255\255\255\255\255\255\255\255\024\001\255\255\
\255\255\255\255\255\255\255\255\030\001\255\255\255\255\255\255\
\034\001\035\001\036\001\037\001\038\001\039\001\255\255\041\001\
\011\001\012\001\013\001\014\001\015\001\255\255\017\001\018\001\
\019\001\255\255\255\255\255\255\255\255\024\001\255\255\255\255\
\255\255\255\255\255\255\030\001\255\255\255\255\255\255\034\001\
\035\001\036\001\037\001\038\001\039\001\255\255\041\001\011\001\
\012\001\013\001\014\001\015\001\255\255\017\001\018\001\019\001\
\255\255\255\255\255\255\255\255\024\001\255\255\255\255\255\255\
\255\255\255\255\030\001\255\255\255\255\255\255\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001\011\001\012\001\013\001\
\014\001\015\001\255\255\017\001\018\001\019\001\255\255\255\255\
\255\255\255\255\024\001\011\001\012\001\013\001\014\001\015\001\
\030\001\017\001\018\001\019\001\034\001\035\001\036\001\037\001\
\038\001\039\001\255\255\255\255\255\255\255\255\030\001\255\255\
\255\255\255\255\034\001\035\001\036\001\037\001\038\001\039\001\
\011\001\012\001\013\001\014\001\015\001\255\255\017\001\018\001\
\019\001\011\001\012\001\013\001\014\001\015\001\255\255\017\001\
\255\255\019\001\255\255\255\255\255\255\255\255\255\255\034\001\
\035\001\036\001\037\001\038\001\039\001\255\255\255\255\255\255\
\034\001\035\001\036\001\037\001\038\001\039\001\011\001\012\001\
\013\001\014\001\015\001\255\255\017\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\034\001\035\001\036\001\
\037\001\038\001\039\001"

let yynames_const = "\
  BOOLEAN_TYPE\000\
  NUMBER_TYPE\000\
  STRING_TYPE\000\
  STREAM_TYPE\000\
  LIST_TYPE\000\
  VOID_TYPE\000\
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
  JUSTDO\000\
  IF\000\
  THEN\000\
  ELSE\000\
  CONS\000\
  HEAD\000\
  TAIL\000\
  EMPTY_LIST\000\
  EMPTY_STREAM\000\
  AS_NUM\000\
  APPLY\000\
  TRUE\000\
  FALSE\000\
  STDIN\000\
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
  SHOWLN\000\
  RANGE\000\
  SPLIT\000\
  SQUARE_BRACE_LEFT\000\
  SQUARE_BRACE_RIGHT\000\
  LET\000\
  SEPARATOR\000\
  STRING_WRAPPER\000\
  ESCAPE_CHAR\000\
  LPAREN\000\
  RPAREN\000\
  COMMENT_LEFT\000\
  COMMENT_RIGHT\000\
  "

let yynames_block = "\
  NUMBER\000\
  IDENT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
             ( _1 )
# 550 "parser.ml"
               : Splat.splTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                        ( SplatNumber )
# 556 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                        ( SplatBoolean )
# 562 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                        ( SplatString )
# 568 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                        ( SplatList )
# 574 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Splat.splType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Splat.splType) in
    Obj.repr(
# 63 "parser.mly"
                                        ( SplatFunction (_2, _3) )
# 582 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Splat.splType) in
    Obj.repr(
# 64 "parser.mly"
                              ( _2 )
# 589 "parser.ml"
               : Splat.splType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                                                                                         ( SplLet (_3, _5, _8) )
# 598 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 70 "parser.mly"
                                    ( SplNumber _1 )
# 605 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                                    ( SplVariable _1 )
# 612 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 72 "parser.mly"
                                           ( SplString _2 )
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 73 "parser.mly"
                                           ( SplString _2 )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                                    ( SplApply (_1, _3) )
# 634 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : Splat.splType) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : Splat.splType) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                                                                                          ( SplAbs (_2, _3, _5, _6, _9) )
# 645 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                                    ( _2 )
# 652 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                    ( SplBoolean false )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                                    ( SplBoolean true )
# 664 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                                    ( SplAnd (_1, _3) )
# 672 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                                    ( SplOr (_1, _3) )
# 680 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                                    ( SplNot _2 )
# 687 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                                    ( SplPlus (_1, _3) )
# 695 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                                    ( SplMinus (_1, _3) )
# 703 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                                    ( SplTimes (_1, _3) )
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                                    ( SplDivide (_1, _3) )
# 719 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                                    ( SplModulo (_1, _3) )
# 727 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                                    ( SplPower (_1, _3) )
# 735 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                                      ( SplLe (_1, _3) )
# 743 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                                      ( SplGe (_1, _3) )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                                      ( SplLt (_1, _3) )
# 759 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                                      ( SplGt (_1, _3) )
# 767 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                      ( SplNe (_1, _3) )
# 775 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                                      ( SplEq (_1, _3) )
# 783 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                                                                                                        ( SplIfElse (_2, _4, _8) )
# 792 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                                    ( SplCons (_1, _3) )
# 800 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
                                    ( SplList [] )
# 806 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                                    ( SplHead _2 )
# 813 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                                    ( SplTail _2 )
# 820 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                                    ( SplList (
        let rec readlines ic =
            try let line = SplString(input_line ic) in
                line :: readlines ic
            with End_of_file -> [] in
        readlines Pervasives.stdin ) )
# 831 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                                    ( SplSplit _2 )
# 838 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                    ( SplAsNum _2 )
# 845 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                                    ( SplShowLn _2 )
# 852 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                                    ( SplShow _2 )
# 859 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'justdo_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                                 ( SplJustDo (_3, _6) )
# 867 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'justdo_expr) in
    Obj.repr(
# 128 "parser.mly"
                                    ( SplJustDo (_1, _3) )
# 875 "parser.ml"
               : 'justdo_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                    ( _1 )
# 882 "parser.ml"
               : 'justdo_expr))
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