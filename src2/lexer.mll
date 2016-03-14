(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule lexer_main = parse
      [' ' '\t' '\n']     { lexer_main lexbuf }     (* skip blanks *)
    | '-'?(['0'-'9']*['.'])?['0'-'9']+ as lsm { NUMBER(float_of_string lsm) }

(*Types*)
    | "boolean"   { BOOLEAN_TYPE }
    | "number"    { NUMBER_TYPE }
    | "string"    { STRING_TYPE }
    | "stream"    { STREAM_TYPE }
    | "list"      { LIST_TYPE }
    | "function"  { FUNCTION_TYPE }

    | "let"    { LET }
    | "in"     { IN }
    | "if"     { IF }
    | "then"   { THEN }
    | "else"   { ELSE }
    | "true"   { TRUE }
    | "false"  { FALSE }
    | ['a'-'z']+ as lxm { IDENT(lxm) }
    | '+'      { PLUS }
    | '-'      { MINUS }
    | '<'      { LESS_THAN }
    | '>'      { GREATER_THAN }
    | ':'      { COLON }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | ':'      { COLON }
    | '='      { EQUALS }
    | "\\l"     { LAMBDA }
    | eof      { EOF }
