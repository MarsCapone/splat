exception LookupError ;;
exception TypeError of string;;
exception UnboundVariableError;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;
exception OutOfBounds ;;
exception SyntaxError ;;

open Printf;;

(* Types of the language *)
type splType =
    SplatNumber
    | SplatBoolean
    | SplatString
    | SplatList
    | SplatVoid
    | SplatFunction of splType * splType

(* Grammar of the language *)
type splTerm =
    | SplNumber of float
    | SplBoolean of bool
    | SplString of string
    | SplList of splTerm list
    | SplVoid of unit
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
(* stream / list operators *)
    | SplCons of splTerm * splTerm
    | SplHead of splTerm
    | SplTail of splTerm
    | SplEmptyList of splTerm
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

(*Function stuff*)
    | SplApply of splTerm * splTerm
    | SplAbs of splType * string * splTerm

let rec isValue e = match e with
    | SplNumber(n) -> true
    | SplBoolean(b) -> true
    | SplList(l) -> true
    | SplVoid(v) -> true
    | SplAbs(tT,x,e') -> true
    | _ -> false
;;



let rec type_to_string tT = match tT with
  | SplatNumber -> "Number"
  | SplatBoolean -> "Boolean"
  | SplatList -> "List"
  | SplatString -> "String"
  | SplatVoid -> "Void"
  | SplatFunction(tT1,tT2) -> "( "^type_to_string(tT1)^" -> "^type_to_string(tT2)^" )"
;;

let rec print_list = function
    [] -> print_string ""
    | SplNumber(e) :: l -> print_float e; print_string " "; print_list l

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
    |SplList (l) -> SplatList

    (*Boolean operators*)
    |SplAnd (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatBoolean, SplatBoolean -> SplatBoolean
        | _ -> raise (TypeError "AND")
    )
    |SplOr (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatBoolean, SplatBoolean -> SplatBoolean
        | _ -> raise (TypeError "OR")
    )
    |SplNot (e1) -> (match (typeOf env e1) with
        SplatBoolean -> SplatBoolean
        | _ -> raise (TypeError "NOT")
    )

    (*Comparisons*)
    |SplLt (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise (TypeError "LESS_THAN")
    )
    |SplGt (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise (TypeError "GREATER_THAN")
    )
    |SplLe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise (TypeError "LESS_THAN_EQUAL")
    )
    |SplGe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise (TypeError "GREATER_THAN_EQUAL")
    )
    |SplNe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise (TypeError "NOT_EQUALS")
    )
    |SplEq (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | _ -> raise (TypeError "EQUALS")
    )

    (*Arithmetic*)
    |SplPlus(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise (TypeError "PLUS")
    )
    |SplMinus(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise (TypeError "MINUS")
    )
    |SplTimes(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise (TypeError "TIMES")
    )
    |SplDivide(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise (TypeError "DIVIDE")
    )
    |SplModulo(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        |_ -> raise (TypeError "MODULO")
    )

    | SplCons(e1, e2) -> (
        match (typeOf env e1), (typeOf env e2) with
            _, SplatList -> SplatList
            | _ -> raise (TypeError "CONS")
    )
    | SplHead (e1) -> (
        typeOf env e1
    )
    | SplTail (e1) -> (
        match (typeOf env e1) with
            SplatList -> SplatList
            | _ -> raise (TypeError "TAIL")
    )

    (*Flow control*)
    |SplIfElse(e1, e2, e3) -> (
        let ty1 = typeOf env e1 in
            match ty1 with
                SplatBoolean -> ( let ty1 = typeOf env e2 in
                    let ty2 = typeOf env e3 in
                    (match (ty1=ty2) with
                        true -> ty1
                        | false -> raise (TypeError "IF_ELSE Internals not same types")
                    ))
                |_ -> raise (TypeError "IF_ELSE Condition not boolean")
    )

    |SplLet(e1, e2, e3) -> (
        let (env') = (addBinding env e1 (typeOf env e2)) in
            (typeOf env' e3)
    )

    |SplApply(e1, e2) -> (
        let ty1 = typeOf env e1 in
        let ty2 = typeOf env e2 in
        (
            match ty1 with
                SplatFunction(tT, tU) ->
                (
                    match tT = ty2 with
                        true -> ty1
                        |false -> raise (TypeError "APPLY Function does not accept type")
                )
                | _ -> raise (TypeError (type_to_string(ty1)^" APPLY "^(type_to_string(ty2))))
        )
    )

    |SplAbs (tT, x, e) ->  (
        let ty1 = typeOf (addBinding env x tT) e in
        (
            match ty1 with
                SplatFunction(p, r) -> ty1
                | _ -> SplatFunction(tT, ty1)
        )
    )

    | SplShow (e1) -> (match (typeOf env e1) with
        SplatNumber -> SplatVoid
        | SplatBoolean -> SplatVoid
        | SplatString -> SplatVoid
        | SplatList -> SplatVoid
        | _ -> raise (TypeError "SHOW")
    )

    |SplVariable (x) ->  (try lookup env x with LookupError -> raise (TypeError x))

let typeProg e = typeOf (Env []) e ;;


let rec eval env e = match e with
  | (SplVariable x) -> (try ((lookup env x) , env) with LookupError -> raise UnboundVariableError)
  | (SplNumber n) -> raise Terminated
  | (SplBoolean b) -> raise Terminated
  | (SplList l) -> raise Terminated
  | (SplAbs(tT,x,e')) -> raise Terminated

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

  | (SplCons(n, SplList(m))) when (isValue(n)) -> (SplList(n :: m ), env)
  | (SplCons(n, SplList(m)))                   -> let (n', env') = (eval env n) in (SplCons(n', SplList(m)), env')
  | (SplCons(n, m)) -> let (m', env') = (eval env m) in (SplCons(n, m'), env')

  | (SplHead(SplList(n))) -> (match n with
        SplNumber(h) :: _ -> (SplNumber(h), env)
        | SplBoolean (h) :: _ -> (SplBoolean(h), env)
        | SplString (h) :: _ -> (SplString(h), env)
        | [] -> raise OutOfBounds
        | _ -> raise SyntaxError
  )
  | (SplHead(e1)) -> let (e1', env') = (eval env e1) in (SplHead (e1'), env')

  | (SplTail(SplList(n))) -> (match n with
        _ :: t -> (SplList(t), env)
        | [] -> raise OutOfBounds
    )
  | (SplTail(e1)) -> let (e1', env') = (eval env e1) in (SplTail(e1'), env')

  (*Assignment*)
  | (SplLet(n, m, e3)) when (isValue(m) )-> (e3, addBinding env n m)
  | (SplLet(n, m, e3)) -> let (m', env') = (eval env m) in (SplLet(n, m', e3), env')

  | (SplApply(SplAbs(tT,x,e), e2)) when (isValue(e2)) -> (e, addBinding env x e2)
  | (SplApply(SplAbs(tT,x,e), e2))                    -> let (e2',env') = (eval env e2) in (SplApply( SplAbs(tT,x,e) , e2') , env')
  | (SplApply(e1,e2))                                -> let (e1',env') = (eval env e1) in (SplApply(e1',e2), env')

  (*Predefined functions*)
  | (SplShow(SplNumber n)) -> (SplVoid(print_float n; print_string "\n"), env)
  | (SplShow(SplBoolean n)) -> (SplVoid(print_string (if n then "true" else "false")), env)
  | (SplShow(SplList(n))) -> (SplVoid(print_list n), env)
  | (SplShow(e1)) -> let (e1', env') = (eval env e1) in (SplShow(e1'), env')

  | _ -> raise Terminated ;;


let rec evalloop env e = try (let (e',env') = (eval env e) in (evalloop env' e')) with Terminated -> if (isValue e) then e else raise StuckTerm;;
let evalProg e = evalloop (Env []) e ;;

let rename (s:string) = s^"'";;





let print_res res = match res with
    | (SplNumber i) -> print_float i ; print_string " : Number"
    | (SplBoolean b) -> print_string (if b then "true" else "false") ; print_string " : Bool"
    | (SplAbs(tT,x,e)) -> print_string("Function : "^type_to_string( typeProg (res) ))
    | (SplList l) -> print_string "["; print_list l; print_string "] : List"
    | (SplVoid v) -> print_string " : Void"
    (*Comment up to raise error to stop debugging*)
    (* | (SplApply(e1, e2)) -> print_string "apply"
    | (SplLet(e1, e2, e3)) -> print_string "let" *)
    | _ -> raise NonBaseTypeResult
