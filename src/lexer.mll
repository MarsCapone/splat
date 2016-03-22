(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule lexer_main = parse
      '<''|'_*'|''>'      { lexer_main lexbuf }
    | [' ' '\t' '\n']     { lexer_main lexbuf }     (* skip blanks *)

(*Variables*)
    | '-'?(['0'-'9']*['.'])?['0'-'9']+ as lsm { NUMBER(float_of_string lsm) }

(*Types*)
    | "boolean"   { BOOLEAN_TYPE }
    | "number"    { NUMBER_TYPE }
    | "string"    { STRING_TYPE }
    | "stream"    { STREAM_TYPE }
    | "list"      { LIST_TYPE }
    | "void"      { VOID_TYPE }
    | "function"  { FUNCTION_TYPE }

(*Flow*)
    | "if"     { IF }
    | "then"   { THEN }
    | "else"   { ELSE }
    | "let"    { LET }
    | '#'      { APPLY }

(*Predefined*)
    | "true"   { TRUE }
    | "false"  { FALSE }
    | "stdin"  { STDIN }
    | "strin"  { STREAMIN }
    | "nextln" { STDIN_STREAMLINE }
    | "strend" { STREAM_END }
    | "lstend" { LIST_END }
    | eof    { EOF }

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
    | "::"      { CONS }
    | "[<>]"    { EMPTY_STREAM }
    | "[]"      { EMPTY_LIST }

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
    | "showln"    { SHOWLN }
    | "show"      { SHOW }
    | "range"     { RANGE }
    | "split"     { SPLIT }
    | "head"      { HEAD }
    | "tail"      { TAIL }
    | "num"       { AS_NUM }

(*Other*)
    | '['     { SQUARE_BRACE_LEFT }
    | ']'     { SQUARE_BRACE_RIGHT }
    | '('     { LPAREN }
    | ')'     { RPAREN }
    | ','     { COMMA }
    | '@'     { AT }
    | ';'     { SEPARATOR }
    | '"'     { STRING_WRAPPER }
    | '\\'    { ESCAPE_CHAR }

    | ['a'-'z''A'-'Z''0'-'9']+ as lxm { IDENT(lxm) }
    | ['^''"']* as str      { STRING(str) }
