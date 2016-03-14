/* File parser.mly */
%{
    open Splat
%}
%token <float> NUMBER
%token <string> IDENT
%token LAMBDA
%token PLUS MINUS TIMES DIVIDE
%token LPAREN RPAREN
%token LET IN EQUALS
%token IF THEN ELSE
%token BOOLEAN_TYPE NUMBER_TYPE FUNCTION_TYPE STRING_TYPE STREAM_TYPE LIST_TYPE
%token TRUE FALSE
%token LESS_THAN GREATER_THAN
%token COLON
%token EOF
%left FUNTYPE
%nonassoc LESS_THAN GREATER_THAN             /* lowest precedence */
%left PLUS MINUS
%nonassoc IF THEN ELSE LET IN /* highest precedence */
%start parser_main             /* the entry point */
%type <Splat.splTerm> parser_main
%type <Splat.splType> type_spec
%%
parser_main:
    expr EOF { $1 }
;
type_spec:
    NUMBER_TYPE { SplatNumber}
    | BOOLEAN_TYPE     { SplatBoolean }
    | type_spec FUNCTION_TYPE type_spec { SplatFunction ($1,$3) }
    | LPAREN type_spec RPAREN {$2}
;
expr:
    NUMBER                      { SplNumber $1 }
    | FALSE                       { SplBoolean false }
    | TRUE                        { SplBoolean true }
    | IDENT                       { SplVariable $1 }
    | expr PLUS expr              { SplPlus ($1, $3) }
    | expr MINUS expr             { SplMinus ($1, $3) }
    | LPAREN expr RPAREN          { $2 }
    | expr LESS_THAN expr         { SplLt ($1, $3) }
    | expr GREATER_THAN expr      { SplGt ($1, $3) }
;
