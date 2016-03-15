/* File parser.mly */
%{
    open Splat
%}
%token <float> NUMBER
%token <string> IDENT
%token <string> STRING
%token BOOLEAN_TYPE NUMBER_TYPE STRING_TYPE STREAM_TYPE LIST_TYPE FUNCTION_TYPE
/*Operators*/
%token PLUS MINUS TIMES DIVIDE MODULO NOT POWER_OF OR AND
%token FOR FOREVER IN
%token IF THEN ELSE
%token WHILE
%token SWITCH
%token BREAK CONTINUE RETURN
%token CONS
%token HEAD TAIL EMPTY_LIST
/*Predefined*/
%token TRUE FALSE
%token STDIN
%token END_OF_STATEMENT EOF
%token EMPTY_LIST
/*Comparators*/
%token LESS_THAN LESS_THAN_EQUAL GREATER_THAN GREATER_THAN_EQUAL EQUAL_TO NOT_EQUAL_TO
/*Scope*/
%token SCOPE_BRACE_LEFT SCOPE_BRACE_RIGHT
/*Assignments*/
%token EQUALS PLUS_EQUALS MINUS_EQUALS MULTIPLY_EQUALS DIVIDE_EQUALS
/*Functions*/
%token SHOW RANGE SPLIT
/*Other*/
%token SQUARE_BRACE_LEFT SQUARE_BRACE_RIGHT LET
%token SEPARATOR
%token STRING_WRAPPER
%token ESCAPE_CHAR
%token LPAREN RPAREN
/*Associativity and precedence*/
%left SEPARATOR             /*Lowest precedence*/
%left CONS
%right HEAD TAIL
%right EQUALS PLUS_EQUALS MINUS_EQUALS MULTIPLY_EQUALS DIVIDE_EQUALS
%left OR
%left AND
%left EQUAL_TO NOT_EQUAL_TO
%left LESS_THAN LESS_THAN_EQUAL GREATER_THAN GREATER_THAN_EQUAL
%left PLUS MINUS
%left DIVIDE MODULO
%left TIMES
%right POWER_OF NOT
%nonassoc IF THEN ELSE WHILE FOR FOREVER IN

%start parser_main             /* the entry point */
%type <Splat.splTerm> parser_main
%type <Splat.splType> type_spec
%%
parser_main:
    expr EOF { $1 }
;
type_spec:
    NUMBER_TYPE         { SplatNumber }
    | BOOLEAN_TYPE      { SplatBoolean }
    | STRING_TYPE       { SplatString }
    | LIST_TYPE         { SplatList }
    | FUNCTION_TYPE type_spec IDENT type_spec    { SplatFunction ($2, $4) }
    | LPAREN type_spec RPAREN { $2 }
;

expr:
    | LET SQUARE_BRACE_LEFT IDENT EQUALS expr SQUARE_BRACE_RIGHT SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT { SplLet ($3, $5, $8) }

    | NUMBER                        { SplNumber $1 }
    | IDENT                         { SplVariable $1 }

    /*Booleans*/
    | FALSE                         { SplBoolean false }
    | TRUE                          { SplBoolean true }
    | expr AND expr                 { SplAnd ($1, $3) }
    | expr OR expr                  { SplOr ($1, $3) }
    | NOT expr                      { SplNot $2 }

    /*Arithmetic*/
    | expr PLUS expr                { SplPlus ($1, $3) }
    | expr MINUS expr               { SplMinus ($1, $3) }
    | expr TIMES expr               { SplTimes ($1, $3) }
    | expr DIVIDE expr              { SplDivide ($1, $3) }
    | expr MODULO expr              { SplModulo ($1, $3) }
    | expr POWER_OF expr            { SplPower ($1, $3) }
    | LPAREN expr RPAREN            { $2 }

    /*Comparisons*/
    | expr LESS_THAN_EQUAL expr       { SplLe ($1, $3) }
    | expr GREATER_THAN_EQUAL expr    { SplGe ($1, $3) }
    | expr LESS_THAN expr             { SplLt ($1, $3) }
    | expr GREATER_THAN expr          { SplGt ($1, $3) }
    | expr NOT_EQUAL_TO expr          { SplNe ($1, $3) }
    | expr EQUAL_TO expr              { SplEq ($1, $3) }

    /*Flow control*/
    | IF expr SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT ELSE SCOPE_BRACE_LEFT expr SCOPE_BRACE_RIGHT      { SplIfElse ($2, $4, $8) }

    /*Stream / List Operations*/
    | expr CONS expr                { SplCons ($1, $3) }
    | EMPTY_LIST                    { SplList [] }
    | HEAD expr                     { SplHead $2 }
    | TAIL expr                     { SplTail $2 }
;
