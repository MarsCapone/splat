(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule white_space = parse
    [' ' '\t']     { white_space lexbuf }     (* skip blanks *)

rule variables = parse
    (\d*[.])?\d+ as lsm { NUMBER(float_of_string lxm) }
    | [a-zA-Z\d]+ as lxm { IDENT(lxm) }

rule types = parse
    "boolean"   { BOOLEAN_TYPE }
    | "number"    { NUMBER_TYPE }
    | "string"    { STRING_TYPE }
    | "stream"    { STREAM_TYPE }
    | "list"      { LIST_TYPE }

rule flow = parse
    "function"  { FUNCTION }
    | "for"       { FOR }
    | "in"     { IN }
    | "if"     { IF }
    | "then"   { THEN }
    | "else"   { ELSE }
    | "switch" { SWITCH }
    | "break"  { BREAK }
    | "continue" { CONTINUE }
    | "return" { RETURN }

rule predefined = parse
    "true"   { TRUE }
    | "false"  { FALSE }
    | "stdin"  { STDIN }
    | '¬'     { END_OF_STATEMENT }
    | eof      { EOF }

rule operators = parse
    '+'      { PLUS }
    | '-'      { MINUS }
    | '*'      { MULTIPLY }
    | '/'      { DIVIDE }
    | '%'      { MODULO }
    | '!'      { NOT }
    | '^'      { POWER_OF }
    | "or"     { OR }
    | "and"    { AND }

rule comparators = parse
    | '<'      { LESS_THAN }
    | '>'      { GREATER_THAN }
    | "<="     { LESS_THAN_EQUAL }
    | ">="     { GREATER_THAN_EQUAL }
    | "=="     { EQUAL_TO }
    | "!="     { NOT_EQUAL_TO }

rule scope = parse
    '{'      { SCOPE_BRACE_LEFT }
    | '}'      { SCOPE_BRACE_RIGHT }

rule assignment = parse
    '='      { EQUALS }
    | "+="     { PLUS_EQUALS }
    | "-="     { MINUS_EQUALS }
    | "*="     { MULTIPLY_EQUALS }
    | "/="     { DIVIDE_EQUALS }

rule functions = parse
    "show"      { SHOW }
    "range"     { RANGE }
    "split"     { SPLIT }

rule other = parse
    '['     { SQUARE_BRACE_LEFT }
    ']'     { SQUARE_BRACE_RIGHT }
    ','     { SEPARATOR }
    '"'     { STRING_WRAPPER }
    '\\'    { ESCAPE_CHAR }
