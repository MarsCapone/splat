(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule lexer_main = parse
      [' ' '\t']     { lexer_main lexbuf }     (* skip blanks *)
    | (\d*[.])?\d+ as lsm { NUMBER(float_of_string lxm) }
    | "boolean"   { BOOLEAN_TYPE }
    | "number"    { NUMBER_TYPE }
    | "string"    { STRING_TYPE }
    | "stream"    { STREAM_TYPE }
    | "function"  { FUNCTION }
    | "for"       { FOR }
    | "in"     { IN }
    | "if"     { IF }
    | "then"   { THEN }
    | "else"   { ELSE }
    | "true"   { TRUE }
    | "false"  { FALSE }
    | ['a'-'z']+ as lxm { IDENT(lxm) }
    | '+'      { PLUS }
    | '-'      { MINUS }
    | '*'      { MULTIPLY }
    | '/'      { DIVIDE }
    | '%'      { MODULO }
    | '!'      { NOT }
    | '^'      { POWER_OF }
    | '<'      { LESS_THAN }
    | '>'      { GREATER_THAN }
    | "<="     { LESS_THAN_EQUAL }
    | ">="     { GREATER_THAN_EQUAL }
    | "=="     { EQUAL_TO }
    | "!="     { NOT_EQUAL_TO }
    | "or"     { OR }
    | "and"    { AND }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | '{'      { SCOPE_BRACE_LEFT }
    | '}'      { SCOPE_BRACE_RIGHT }
    | '='      { EQUALS }
    | "+="     { PLUS_EQUALS }
    | "-="     { MINUS_EQUALS }
    | "*="     { MULTIPLY_EQUALS }
    | "/="     { DIVIDE_EQUALS }
    | '\n'     { NEW_LINE }
    | eof      { EOF }
