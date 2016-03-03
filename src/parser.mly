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
;

boolean_expr:
    TRUE                                    { splBoolean true }
    | FALSE                                 { splBoolean false }
    | boolean_expr AND boolean_expr         { splAnd ($1, $3) }
    | boolean_expr OR boolean_expr          { splOr ($1, $3) }
    | NOT boolean_expr                      { splNot $2 }
    | comparator_expr
;

comparator_expr:
    math_expr LESS_THAN_EQUAL math_expr     { splLe ($1, $3) }
    math_expr GREATER_THAN_EQUAL math_expr  { splGe ($1, $3) }
;

math_expr:
    NUMBER                                  { splNum $1 }
    | math_expr DIVIDE math_expr            { splDivide ($1, $3) }
    | math_expr MULTIPLY math_expr          { splTimes ($1, $3) }
    | math_expr MINUS math_expr             { splMinus ($1, $3) }
    | math_expr PLUS math_expr              { splPlus ($1, $3) }
    | math_expr MODULO math_expr            { splModulo ($1, $3) }
    | math_expr POWER_OF math_expr          { splPower ($1, $3) }
;

switch_contents_expr:
(* Not sure if this is right, but how else to do it? *)
    comparator_expr SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT switch_contents_expr { splIf ($1, $3) }
    | SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT       { $2 }

flow_expr:
    FOR expr IN expr SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT { splFor ($2, $4, $6)}
    | FOREVER SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT     { splForever $3 }
    | WHILE expr SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT  { splWhile ($2, $4) }
    | IF expr SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT ELSE SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT      { splIfElse ($2, $4, $8) }
    | IF expr SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT       { splIf ($2, $4) }
    | SWITCH expr SCOPE_BRACE_LEFT switch_contents_expr SCOPE_BRACE_RIGHT   { splSwitch ($2, $4) }
;
