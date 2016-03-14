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

val parser_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Splat.splTerm
