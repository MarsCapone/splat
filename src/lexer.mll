(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule lexer_main = parse
    [' ' '\t']     { white_space lexbuf }     (* skip blanks *)
    | '-'?(['0'-'9']*['.'])?['0'-'9']+ as lsm { NUMBER(float_of_string lxm) } (*Variables*)
    | ['a'-'z''A'-'Z''0'-'9']+ as lxm { IDENT(lxm) }

(*Types*)
    | "boolean"   { BOOLEAN_TYPE }
    | "number"    { NUMBER_TYPE }
    | "string"    { STRING_TYPE }
    | "stream"    { STREAM_TYPE }
    | "list"      { LIST_TYPE }
    | "function"  { FUNCTION_TYPE }

(*Flow*)
    | "function"  { FUNCTION }
    | "for"     { FOR }
    | "forever" { FOREVER }
    | "in"     { IN }
    | "if"     { IF }
    | "then"   { THEN }
    | "else"   { ELSE }
    | "while"  { WHILE }
    | "switch" { SWITCH }
    | "break"  { BREAK }
    | "continue" { CONTINUE }
    | "return" { RETURN }

(*Predefined*)
    | "true"   { TRUE }
    | "false"  { FALSE }
    | "stdin"  { STDIN }
    | "¬"    { END_OF_STATEMENT }
    | eof      { EOF }

(*Operators*)
    | '+'      { PLUS }
    | '-'      { MINUS }
    | '*'      { MULTIPLY }
    | '/'      { DIVIDE }
    | '%'      { MODULO }
    | '!'      { NOT }
    | '^'      { POWER_OF }
    | "or"     { OR }
    | "and"    { AND }

(*Comparators*)
    | '<'      { LESS_THAN }
    | '>'      { GREATER_THAN }
    | "<="     { LESS_THAN_EQUAL }
    | ">="     { GREATER_THAN_EQUAL }
    | "=="     { EQUAL_TO }
    | "!="     { NOT_EQUAL_TO }

(*Scope*)
    | '{'      { SCOPE_BRACE_LEFT }
    | '}'      { SCOPE_BRACE_RIGHT }

(*Assignment*)
    | '='      { EQUALS }
    | "+="     { PLUS_EQUALS }
    | "-="     { MINUS_EQUALS }
    | "*="     { MULTIPLY_EQUALS }
    | "/="     { DIVIDE_EQUALS }

(*Functions*)
    | "show"      { SHOW }
    | "range"     { RANGE }
    | "split"     { SPLIT }

(*Other*)
    | '['     { SQUARE_BRACE_LEFT }
    | ']'     { SQUARE_BRACE_RIGHT }
    | '('     { LPAREN }
    | ')'     { RPAREN }
    | ','     { SEPARATOR }
    | '"'     { STRING_WRAPPER }
    | '\\'    { ESCAPE_CHAR }
