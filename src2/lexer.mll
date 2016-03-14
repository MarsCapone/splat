(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule lexer_main = parse
    [' ' '\t' '\n']     { lexer_main lexbuf }     (* skip blanks *)

(*Variables*)
    | '-'?(['0'-'9']*['.'])?['0'-'9']+ as lsm { NUMBER(float_of_string lsm) }

(*Types*)
    | "boolean"   { BOOLEAN_TYPE }
    | "number"    { NUMBER_TYPE }
    | "string"    { STRING_TYPE }
    | "stream"    { STREAM_TYPE }
    | "list"      { LIST_TYPE }
    | "function"  { FUNCTION_TYPE }

(*Flow*)
    | "forever" { FOREVER }
    | "for"     { FOR }
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
    | "¬"      { END_OF_STATEMENT }
    | eof      { EOF }

(*Operators*)
    | '+'      { PLUS }
    | '-'      { MINUS }
    | '*'      { TIMES }
    | '/'      { DIVIDE }
    | '%'      { MODULO }
    | "not"      { NOT }
    | '^'      { POWER_OF }
    | "or"     { OR }
    | "and"    { AND }

(*Comparators*)
    | "<="     { LESS_THAN_EQUAL }
    | ">="     { GREATER_THAN_EQUAL }
    | "=="     { EQUAL_TO }
    | "!="     { NOT_EQUAL_TO }
    | '<'      { LESS_THAN }
    | '>'      { GREATER_THAN }

(*Scope*)
    | '{'      { SCOPE_BRACE_LEFT }
    | '}'      { SCOPE_BRACE_RIGHT }

(*Assignment*)
    | "+="     { PLUS_EQUALS }
    | "-="     { MINUS_EQUALS }
    | "*="     { MULTIPLY_EQUALS }
    | "/="     { DIVIDE_EQUALS }
    | '='      { EQUALS }

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

    | ['a'-'z''A'-'Z''0'-'9']+ as lxm { IDENT(lxm) }
