/* File parser.mly */
%{
    open Splat
%}
%token <float> NUMBER
%token <string> IDENT
%token LAMBDA
%token PLUS MINUS
%token LPAREN RPAREN
%token LET IN EQUALS
%token IF THEN ELSE
%token BOOLEAN_TYPE NUMBER_TYPE FUNTYPE
%token TRUE FALSE
%token LESSTHAN
%token COLON
%token EOF
%left FUNTYPE
%left PLUS MINUS           /* lowest precedence */
%nonassoc LESSTHAN
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
    | type_spec FUNTYPE type_spec { SplatFunction ($1,$3) }
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
;
