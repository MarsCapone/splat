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
    | SplLet of string * splTerm * splTerm
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
    |SplString (s) -> SplatString

    (*Boolean operators*)
    |SplAnd (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatBoolean, SplatBoolean -> SplatBoolean
        | _ -> raise TypeError
    )
    |SplOr (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatBoolean, SplatBoolean -> SplatBoolean
        | _ -> raise TypeError
    )
    |SplNot (e1) -> (match (typeOf env e1) with
        SplatBoolean -> SplatBoolean
        | _ -> raise TypeError
    )

    (*Comparisons*)
    |SplLt (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise TypeError
    )
    |SplGt (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise TypeError
    )
    |SplLe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise TypeError
    )
    |SplGe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise TypeError
    )
    |SplNe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise TypeError
    )
    |SplEq (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise TypeError
    )

    (*Arithmetic*)
    |SplPlus(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise TypeError
    )
    |SplMinus(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise TypeError
    )
    |SplTimes(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise TypeError
    )
    |SplDivide(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise TypeError
    )
    |SplModulo(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise TypeError
    )

    (*Flow control*)
    |SplIfElse(e1, e2, e3) -> (
        match (typeOf env e1) , (typeOf env e2), (typeOf env e3) with
              SplatBoolean, SplatNumber, SplatNumber -> SplatNumber
            | SplatBoolean, SplatBoolean, SplatBoolean -> SplatBoolean
            | _ -> raise TypeError
    )

    |SplLet(e1, e2, e3) -> (
        let (env') = (addBinding env e1 (typeOf env e2)) in
            (typeOf env' e3)
    )

    |SplVariable (x) ->  (try lookup env x with LookupError -> raise TypeError)

let typeProg e = typeOf (Env []) e ;;


let rec eval env e = match e with
  | (SplVariable x) -> (try ((lookup env x) , env) with LookupError -> raise UnboundVariableError)
  | (SplNumber n) -> raise Terminated
  | (SplBoolean b) -> raise Terminated

  (*Boolean operators*)
  | (SplAnd(SplBoolean(n),SplBoolean(m))) -> (SplBoolean( n && m ) , env)
  | (SplAnd(SplBoolean(n), e2))      -> let (e2',env') = (eval env e2) in (SplAnd(SplBoolean(n),e2'),env')
  | (SplAnd(e1, e2))            -> let (e1',env') = (eval env e1) in (SplAnd(e1',e2),env')

  | (SplOr(SplBoolean(n),SplBoolean(m))) -> (SplBoolean( n || m ) , env)
  | (SplOr(SplBoolean(n), e2))      -> let (e2',env') = (eval env e2) in (SplOr(SplBoolean(n),e2'),env')
  | (SplOr(e1, e2))            -> let (e1',env') = (eval env e1) in (SplOr(e1',e2),env')

  | (SplNot(SplBoolean(n))) -> (SplBoolean( not n ) , env)
  | (SplNot(e1))      -> let (e1',env') = (eval env e1) in (SplNot(e1'),env')

  (*Comparisons*)
  | (SplLt(SplNumber(n),SplNumber(m))) -> (SplBoolean( n < m ) , env)
  | (SplLt(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplLt(SplNumber(n),e2'),env')
  | (SplLt(e1, e2))            -> let (e1',env') = (eval env e1) in (SplLt(e1',e2),env')

  | (SplGt(SplNumber(n),SplNumber(m))) -> (SplBoolean( n > m ) , env)
  | (SplGt(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplGt(SplNumber(n),e2'),env')
  | (SplGt(e1, e2))            -> let (e1',env') = (eval env e1) in (SplGt(e1',e2),env')

  | (SplLe(SplNumber(n),SplNumber(m))) -> (SplBoolean( n <= m ) , env)
  | (SplLe(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplLe(SplNumber(n),e2'),env')
  | (SplLe(e1, e2))            -> let (e1',env') = (eval env e1) in (SplLe(e1',e2),env')

  | (SplGe(SplNumber(n),SplNumber(m))) -> (SplBoolean( n >= m ) , env)
  | (SplGe(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplGe(SplNumber(n),e2'),env')
  | (SplGe(e1, e2))            -> let (e1',env') = (eval env e1) in (SplGe(e1',e2),env')

  | (SplNe(SplNumber(n),SplNumber(m))) -> (SplBoolean( n != m ) , env)
  | (SplNe(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplNe(SplNumber(n),e2'),env')
  | (SplNe(e1, e2))            -> let (e1',env') = (eval env e1) in (SplNe(e1',e2),env')

  | (SplEq(SplNumber(n),SplNumber(m))) -> (SplBoolean( n = m ) , env)
  | (SplEq(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplEq(SplNumber(n),e2'),env')
  | (SplEq(e1, e2))            -> let (e1',env') = (eval env e1) in (SplEq(e1',e2),env')
  (*TODO?: Boolean equals / not_equals*)

  (*Arithmetic*)
  | (SplPlus(SplNumber(n),SplNumber(m))) -> (SplNumber( n +. m ) , env)
  | (SplPlus(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplPlus(SplNumber(n),e2'),env')
  | (SplPlus(e1, e2))            -> let (e1',env') = (eval env e1) in (SplPlus(e1', e2) ,env')

  | (SplMinus(SplNumber(n),SplNumber(m))) -> (SplNumber( n -. m ) , env)
  | (SplMinus(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplMinus(SplNumber(n),e2'),env')
  | (SplMinus(e1, e2))            -> let (e1',env') = (eval env e1) in (SplMinus(e1', e2) ,env')

  | (SplTimes(SplNumber(n),SplNumber(m))) -> (SplNumber( n *. m ) , env)
  | (SplTimes(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplTimes(SplNumber(n),e2'),env')
  | (SplTimes(e1, e2))            -> let (e1',env') = (eval env e1) in (SplTimes(e1', e2) ,env')

  | (SplDivide(SplNumber(n),SplNumber(m))) -> (SplNumber( n /. m ) , env)
  | (SplDivide(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplDivide(SplNumber(n),e2'),env')
  | (SplDivide(e1, e2))            -> let (e1',env') = (eval env e1) in (SplDivide(e1', e2) ,env')

  | (SplModulo(SplNumber(n),SplNumber(m))) -> (SplNumber( mod_float n m ) , env)
  | (SplModulo(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplModulo(SplNumber(n),e2'),env')
  | (SplModulo(e1, e2))            -> let (e1',env') = (eval env e1) in (SplModulo(e1', e2) ,env')

  | (SplPower(SplNumber(n),SplNumber(m))) -> (SplNumber( n ** m ) , env)
  | (SplPower(SplNumber(n), e2))      -> let (e2',env') = (eval env e2) in (SplPower(SplNumber(n),e2'),env')
  | (SplPower(e1, e2))            -> let (e1',env') = (eval env e1) in (SplPower(e1', e2) ,env')

  (*Flow*)
  | (SplIfElse(SplBoolean(n), SplNumber(m), SplNumber(o))) -> (SplNumber( if n then m else o), env)
  | (SplIfElse(SplBoolean(n), SplBoolean(m), SplBoolean(o))) -> (SplBoolean( if n then m else o), env)
  | (SplIfElse(SplBoolean(n), SplNumber(m), e3))      -> let (e3',env') = (eval env e3) in (SplIfElse(SplBoolean(n),SplNumber(m),e3),env')
  | (SplIfElse(SplBoolean(n), SplBoolean(m), e3))      -> let (e3',env') = (eval env e3) in (SplIfElse(SplBoolean(n),SplBoolean(m),e3),env')
  | (SplIfElse(SplBoolean(n), e2, e3))      -> let (e2',env') = (eval env e2) in (SplIfElse(SplBoolean(n),e2',e3),env')
  | (SplIfElse(e1, e2, e3))            -> let (e1',env') = (eval env e1) in (SplIfElse(e1', e2, e3) ,env')

  (*Assignment*)
  | (SplLet(n, SplNumber(m), e3)) -> let (env') = (addBinding env n (SplNumber(m))) in (e3, env')
  | (SplLet(n, m, e3)) -> let (m', env') = (eval env m) in (SplLet(n, m', e3), env')

  | _ -> raise Terminated ;;


let rec evalloop env e = try (let (e',env') = (eval env e) in (evalloop env' e')) with Terminated -> if (isValue e) then e else raise StuckTerm;;
let evalProg e = evalloop (Env []) e ;;

let rename (s:string) = s^"'";;

let rec type_to_string tT = match tT with
  | SplatNumber -> "Number"
  | SplatBoolean -> "Boolean"
  | SplatList -> "List"
  | SplatStream -> "Stream"
  | SplatString -> "String"
  | SplatFunction(tT1,tT2) -> "( "^type_to_string(tT1)^" -> "^type_to_string(tT2)^" )"
;;

let print_res res = match res with
    | (SplNumber i) -> print_float i ; print_string " : Number"
    | (SplBoolean b) -> print_string (if b then "true" else "false") ; print_string " : Bool"
    | _ -> raise NonBaseTypeResult
