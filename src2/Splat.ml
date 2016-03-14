exception LookupError ;;
exception TypeError ;;
exception UnboundVariableError;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;

(* Types of the language *)
type splType =
    SplatNumber
    | SplatBoolean
    | SplatString
    | SplatList
    | SplatStream
    | SplatFunction of splType * splType

(* Grammar of the language *)
type splTerm =
    | SplNumber of float
    | SplBoolean of bool
    | SplString of string
    | SplVariable of string
(* number operators *)
    | SplPlus of splTerm * splTerm
    | SplMinus of splTerm * splTerm
    | SplTimes of splTerm * splTerm
    | SplDivide of splTerm * splTerm
    | SplModulo of splTerm * splTerm
    | SplPower of splTerm * splTerm
(* boolean operators *)
    | SplNot of splTerm
    | SplAnd of splTerm * splTerm
    | SplOr of splTerm * splTerm
(* flow *)
    | SplFor of splTerm * splTerm * splTerm
    | SplForever of splTerm
    | SplWhile of splTerm * splTerm
    | SplIfElse of splTerm * splTerm * splTerm
    | SplIf of splTerm * splTerm
    | SplSwitch of splTerm * splTerm
(* comparators *)
    | SplLt of splTerm * splTerm
    | SplGt of splTerm * splTerm
    | SplLe of splTerm * splTerm
    | SplGe of splTerm * splTerm
    | SplEq of splTerm * splTerm
    | SplNe of splTerm * splTerm
(* assignment *)
    | SplAssign of string * splTerm
    | SplPlusAssign of string * splTerm
    | SplMinusAssign of string * splTerm
    | SplTimesAssign of string * splTerm
    | SplDivideAssign of string * splTerm
(* predefined functions *)
    | SplShow of splTerm
    | SplRange of splTerm * splTerm * splTerm
    | SplSplit of splTerm

let rec isValue e = match e with
    | SplNumber(n) -> true
    | SplBoolean(b) -> true
    | _ -> false
;;

(* Type of Environments *)

type 'a context = Env of (string * 'a) list
type typeContext = splType context
type valContext = splTerm context

(* Function to look up the type of a string name variable in a type environment *)
let rec lookup env str = match env with
   Env [] -> raise LookupError
  |Env ((name,thing) :: gs) ->
        (
          match (name = str) with
            true -> thing
           |false -> lookup (Env (gs)) str
	)
;;

(* Function to add an extra entry in to an environment *)
let addBinding env str thing = match env with
      Env(gs) -> Env ( (str, thing) :: gs ) ;;


(* The type checking function itself *)
let rec typeOf env e = match e with
    SplNumber (n) -> SplatNumber
    |SplBoolean (b) -> SplatBoolean

    |SplLt (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
            | _ -> raise TypeError
    )
    |SplGt (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
            | _ -> raise TypeError
    )

    |SplPlus(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
            |_ -> raise TypeError
    )
    |SplMinus(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
            |_ -> raise TypeError
    )

    |SplVariable (x) ->  (try lookup env x with LookupError -> raise TypeError)

let typeProg e = typeOf (Env []) e ;;


let rec eval env e = match e with
  | (SplVariable x) -> (try ((lookup env x) , env) with LookupError -> raise UnboundVariableError)
  | (SplNumber n) -> raise Terminated
  | (SplBoolean b) -> raise Terminated

  | (SplLt(SplNumber(n),SplNumber(m))) -> (SplBoolean( n < m ) , env)
  | (SplLt(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplLt(SplNumber(n),e2'),env')
  | (SplLt(e1, e2))            -> let (e1',env') = (eval env e1) in (SplLt(e1',e2),env')

  | (SplGt(SplNumber(n),SplNumber(m))) -> (SplBoolean( n > m ) , env)
  | (SplGt(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplGt(SplNumber(n),e2'),env')
  | (SplGt(e1, e2))            -> let (e1',env') = (eval env e1) in (SplGt(e1',e2),env')

  | (SplPlus(SplNumber(n),SplNumber(m))) -> (SplNumber( n +. m ) , env)
  | (SplPlus(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplPlus(SplNumber(n),e2'),env')
  | (SplPlus(e1, e2))            -> let (e1',env') = (eval env e1) in (SplPlus(e1', e2) ,env')

  | (SplMinus(SplNumber(n),SplNumber(m))) -> (SplNumber( n -. m ) , env)
  | (SplMinus(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplMinus(SplNumber(n),e2'),env')
  | (SplMinus(e1, e2))            -> let (e1',env') = (eval env e1) in (SplMinus(e1', e2) ,env')

  | _ -> raise Terminated ;;


let rec evalloop env e = try (let (e',env') = (eval env e) in (evalloop env' e')) with Terminated -> if (isValue e) then e else raise StuckTerm  ;;
let evalProg e = evalloop (Env []) e ;;

let rec free e x = match e with
   SplVariable(y) -> (x=y)
  |SplNumber(n) -> false
  |SplBoolean(b) -> false
  |SplLt(e1,e2) -> (free e1 x) || (free e2 x)
  |SplGt(e1,e2) -> (free e1 x) || (free e2 x)
  |SplPlus(e1,e2) -> (free e1 x) || (free e2 x)
  |SplMinus(e1,e2) -> (free e1 x) || (free e2 x)
;;

let rename (s:string) = s^"'";;

let rec subst e1 x e2 = match e2 with
    SplVariable(y) when (x=y) -> e1
  | SplVariable(y)            -> SplVariable(y)
  | SplNumber(n) -> SplNumber(n)
  | SplBoolean(b) -> SplBoolean(b)
  | SplLt (e21, e22) -> SplLt( (subst e1 x e21) , (subst e1 x e22) )
  | SplGt (e21, e22) -> SplGt( (subst e1 x e21) , (subst e1 x e22) )
  | SplPlus(e21, e22) -> SplPlus( (subst e1 x e21) , (subst e1 x e22) )
  | SplMinus(e21, e22) -> SplMinus( (subst e1 x e21) , (subst e1 x e22) )
 ;;

let rec type_to_string tT = match tT with
  | SplatNumber -> "Number"
  | SplatBoolean -> "Boolean"
  | SplatFunction(tT1,tT2) -> "( "^type_to_string(tT1)^" -> "^type_to_string(tT2)^" )"
;;

let print_res res = match res with
    | (SplNumber i) -> print_float i ; print_string " : Number"
    | (SplBoolean b) -> print_string (if b then "true" else "false") ; print_string " : Bool"
    | _ -> raise NonBaseTypeResult
