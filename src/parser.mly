/* File parser.mly */
%{
    open Splat
%}
%token <float> NUMBER
%token <string> IDENT
%token BOOLEAN_TYPE NUMBER_TYPE STRING_TYPE STREAM_TYPE LIST_TYPE FUNCTION_TYPE
/*Operators*/
%token PLUS MINUS MULTIPLY DIVIDE MODULO NOT POWER_OF OR AND
%token FOR FOREVER IN
%token IF THEN ELSE
%token WHILE
%token SWITCH
%token BREAK CONTINUE RETURN
/*Predefined*/
%token TRUE FALSE
%token STDIN
%token END_OF_STATEMENT EOF
/*Comparators*/
%token LESS_THAN LESS_THAN_EQUAL GREATER_THAN GREATER_THAN_EQUAL EQUAL_TO NOT_EQUAL_TO
/*Scope*/
%token SCOPE_BRACE_LEFT SCOPE_BRACE_RIGHT
/*Assignments*/
%token EQUALS PLUS_EQUALS MINUS_EQUALS MULTIPLY_EQUALS DIVIDE_EQUALS
/*Functions*/
%token SHOW RANGE SPLIT
/*Other*/
%token SQUARE_BRACE_LEFT SQUARE_BRACE_RIGHT
%token SEPARATOR
%token STRING_WRAPPER
%token ESCAPE_CHAR
/*Associativity and precedence*/
%left SEPARATOR             /*Lowest precedence*/
%right EQUALS PLUS_EQUALS MINUS_EQUALS MULTIPLY_EQUALS DIVIDE_EQUALS
%left OR
%left AND
%left EQUAL_TO NOT_EQUAL_TO
%left LESS_THAN LESS_THAN_EQUAL GREATER_THAN GREATER_THAN_EQUAL
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO
%right POWER_OF NOT
%nonassoc IF THEN ELSE WHILE FOR FOREVER IN

%start parser_main
%type <Splat.splatTerm> parser_main
%type <Splat.splatType> type_spec
%%
parser_main: expr EOF { $1 }
;
type_spec:
    NUMBER_TYPE         { splatNumber }
    | BOOLEAN_TYPE      { splatBoolean }
    | STRING_TYPE       { splatString }
    | LIST_TYPE         { splatList }
    | STREAM_TYPE       { splatStream }
    | FUNCTION_TYPE     { splatFunction }
    | LPAREN type_spec RPAREN { $2 }
