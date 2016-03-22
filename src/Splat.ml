exception LookupError of string;;
exception TypeError of string;;
exception UnboundVariableError of string;;
exception Terminated ;;
exception NonBaseTypeResult of string;;
exception OutOfBounds of string ;;
exception SyntaxError of string ;;
exception ConversionError of string ;;
exception FunctionError of string ;;

open Printf;;

(* Types of the language *)
type splType =
    SplatNumber
    | SplatBoolean
    | SplatString
    | SplatStream
    | SplatList of splType
    | SplatFunction of splType * splType

(* Grammar of the language *)
type splTerm =
    | SplNumber of float
    | SplBoolean of bool
    | SplString of string
    | SplList of splTerm list
    | SplStream of string Stream.t
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
    | SplStreamEnd of splTerm
(* flow *)
    | SplIfElse of splTerm * splTerm list * splTerm list
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
    | SplLet of string * splTerm * splTerm list
(* predefined functions *)
    | SplShow of splTerm
    | SplShowLn of splTerm
    | SplRange of splTerm * splTerm * splTerm
    | SplSplit of splTerm
    | SplAsNum of splTerm

(*Function stuff*)
    | SplApply of splTerm * splTerm
    | SplAbs of splType * string * splType * string * splTerm list

exception StuckTerm of splTerm;;

let rec isValue e = match e with
    | (SplNumber n) -> true
    | (SplBoolean b) -> true
    | (SplString s) -> true
    | (SplList l) -> true
    | (SplStream s) -> true
    | (SplAbs (rT, n, tT,x,e')) -> true
    | _ -> false
;;

let rec type_to_string tT = match tT with
  | SplatNumber -> "Number"
  | SplatBoolean -> "Boolean"
  | SplatList(t) -> ("List of "^type_to_string(t))
  | SplatStream -> "Stream"
  | SplatString -> "String"
  | SplatFunction(tT1,tT2) -> "( "^type_to_string(tT1)^" -> "^type_to_string(tT2)^" )"
;;

let rec print_list = function
    [] -> print_string "[]"
    | SplNumber(e) :: l -> print_float e; print_string "::"; print_list l
    | SplBoolean(b) :: l -> print_string (if b then "true::" else "false::"); print_list l
    | SplList(a) :: l -> print_string "("; print_list a; print_string ")"; print_list l
    | SplString(s) :: l -> print_string s; print_string "::"; print_list l
    | SplAbs(rT, n, tT, x, e) :: l -> print_string "function:";
        print_string (type_to_string tT); print_list l
    | _ -> print_string "null"


let rec split s =
    SplList(let tokens = (Str.split (Str.regexp " ") s) in
    let rec convert rem =
        if rem=[] then
            []
        else
            let v = SplString(List.hd rem) in
                v :: convert (List.tl rem)
    in convert tokens)


(* Type of Environments *)

type 'a context = Env of (string * 'a) list
type typeContext = splType context
type valContext = splTerm context

(* Function to look up the type of a string name variable in a type environment *)
let rec lookup env str = match env with
   Env [] -> raise (LookupError ("Lookup value was not in environment: "^str))
  |Env ((name,thing) :: gs) ->
        (
          match (name = str) with
            true -> thing
           |false -> lookup (Env (gs)) str
	)
;;

let rec last = function
    | x :: [] -> x
    | _ :: xs -> last xs
    | [] -> raise (OutOfBounds "List is empty")
;;

(* Function to add an extra entry in to an environment *)
let addBinding env str thing = match env with
      Env(gs) -> Env ( (str, thing) :: gs ) ;;

let typechecking_on = false ;;

(* The type checking function itself *)
let rec typeOf env e = match e with
    SplNumber (n) -> SplatNumber
    |SplBoolean (b) -> SplatBoolean
    |SplString (s) -> SplatString
    |SplList (l) -> (match l with 
        [] -> SplatList (SplatNumber)
        | SplNumber(n) :: _ -> SplatList (SplatNumber)
        | SplBoolean(b) :: _ -> SplatList (SplatBoolean)
        | SplString(s) :: _ -> SplatList (SplatString)
        | _ -> raise (TypeError "Invalid types: LIST")
    )
    | SplStream (s) -> SplatStream 

    (*Boolean operators*)
    |SplAnd (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatBoolean, SplatBoolean -> SplatBoolean
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" AND "^type_to_string(b)))
    )
    |SplOr (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatBoolean, SplatBoolean -> SplatBoolean
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" OR "^type_to_string(b)))
    )
    |SplNot (e1) -> (match (typeOf env e1) with
        SplatBoolean -> SplatBoolean
        | a -> raise (TypeError ("Invalid type: NOT "^type_to_string(a)))
    )

    (*Comparisons*)
    |SplLt (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" LESS_THAN "^type_to_string(b)))
    )
    |SplGt (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" GREATER_THAN "^type_to_string(b)))
    )
    |SplLe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" LESS_THAN_EQUAL "^type_to_string(b)))
    )
    |SplGe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" GREATER_THAN_EQUAL "^type_to_string(b)))
    )
    |SplNe (e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatBoolean
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" NOT_EQUALS "^type_to_string(b)))
    )
    |SplEq (e1,e2) -> (
        let ty1 = typeOf env e1 in
        let ty2 = typeOf env e2 in
        (match (ty1=ty2) with
            true -> SplatBoolean
            | false -> raise (TypeError ("Invalid types: "
                ^type_to_string(ty1)^" EQUALS "^type_to_string(ty2)))
        )
    )

    (*Arithmetic*)
    | SplPlus(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" PLUS "^type_to_string(b)))
    )
    | SplMinus(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" MINUS "^type_to_string(b)))
    )
    | SplTimes(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" TIMES "^type_to_string(b)))
    )
    | SplDivide(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" DIVIDE "^type_to_string(b)))
    )
    | SplModulo(e1,e2) -> (match (typeOf env e1) , (typeOf env e2) with
        SplatNumber, SplatNumber -> SplatNumber
        | a, b -> raise (TypeError ("Invalid types: "
            ^type_to_string(a)^" MODULO "^type_to_string(b)))
    )

    | SplCons(e1, e2) -> (
        match (typeOf env e1), (typeOf env e2) with
            a, SplatList(n) -> (
                match a=n with
                    true -> SplatList(n)
                    | false -> raise (TypeError ("Invalid types: "
                ^type_to_string(a)^" CONS "^type_to_string(n))))
            | _ -> raise (TypeError "Invalid types: CONSTRUCT type and list
type do not match")
    )
    | SplHead (e1) -> (
        match (typeOf env e1) with 
            SplatList(n) -> n
            | _ -> raise (TypeError "Invalid type: HEAD parameter is not a list")
    )

    | SplTail (e1) -> (
        match (typeOf env e1) with
            SplatList(n) -> SplatList(n)
            | a -> raise (TypeError ("Invalid type: TAIL "^type_to_string(a)))
    )

    | SplStreamEnd (e1) -> (
        match (typeOf env e1) with
            SplatString -> SplatBoolean 
            | SplatNumber -> SplatBoolean
            | t -> raise (TypeError ("Invalid type: Stream End can only be String, not "^(type_to_string(t))))
    )

    | SplEmptyList (e1) -> (
        match (typeOf env e1) with
            SplatList(n) -> SplatBoolean 
            | t -> raise (TypeError ("Invalid type: EMPTYLIST "^type_to_string(t)))
    )
    
    (*Flow control*)
    |SplIfElse(e1, e2, e3) -> (
        let ty1 = typeOf env e1 in
            match ty1 with
                SplatBoolean -> (
                    let ty1 = typeOf env (last e2) in
                    let ty2 = typeOf env (last e3) in
                    (match (ty1=ty2) with
                        true -> ty1
                        | false -> (
                            match ty1 with
                                SplatFunction(tT, tU) -> ty2
                                | a -> (
                                    match ty2 with
                                        SplatFunction(tT, tU) -> ty1
                                        | b -> raise (TypeError
                                        ("Invalid types: "
                                        ^"Internals do not match: "
                                        ^"IF "^type_to_string(a)
                                        ^" ELSE "^type_to_string(b)))
                                    )
                            )
                    ))
                | c -> raise (TypeError
                    ("Invalid type: Condition is not BOOLEAN: "
                    ^type_to_string(c)))
    )

    |SplLet(e1, e2, e3) -> (
        let (env') = (addBinding env e1 (typeOf env e2)) in
            (typeOf env' (last e3))
    )

    |SplApply(e1, e2) -> (
        let ty1 = typeOf env e1 in
        let ty2 = typeOf env e2 in
        (
            match ty1 with
                SplatFunction(tT, tU) ->
                (
                    (*(print_string ("Apply "^type_to_string(ty2)^" to function
                        * accepting "^type_to_string(tT)^"\n"));*)
                    match tT = ty2 with
                        true -> tU
                        | false -> raise (TypeError ("Invalid types: Function expected "
                            ^type_to_string(tT)^" but received "^type_to_string(ty2)))
                )
                | _ -> if typechecking_on then
                            raise (TypeError ("Invalid types: "
                                ^type_to_string(ty1)^" APPLY "^(type_to_string(ty2))))
                        else
                            ty1
        )
    )

    |SplAbs (rT, n, tT, x, e) ->  (
        let env' = (addBinding (addBinding env n (SplatFunction(tT, rT))) x tT ) in
        let ty1 = typeOf env' (last e) in
        (
            match ty1 with
                SplatFunction(p, r) -> SplatFunction(tT, SplatFunction(p, r))
                | _ -> SplatFunction(tT, rT)
        )
    )

    | SplShow (e1) -> (match (typeOf env e1) with
        SplatNumber -> SplatNumber
        | SplatBoolean -> SplatBoolean
        | SplatString -> SplatString
        | SplatList(n) -> SplatList(n)
        | a -> raise (TypeError ("Invalid type: SHOW "^type_to_string(a)))
    )

    | SplShowLn (e1) -> (match (typeOf env e1) with
        SplatNumber -> SplatNumber
        | SplatBoolean -> SplatBoolean
        | SplatString -> SplatString
        | SplatList(n) -> SplatList(n)
        | a -> raise (TypeError ("Invalid type: SHOWLN"^type_to_string(a)))
    )

    | SplSplit (e1) -> (
        let ty1 = typeOf env e1 in
        match ty1 with
            SplatString -> SplatString
            | _ -> raise (TypeError ("Invalid type: Cannot SPLIT "
                ^(type_to_string(ty1))))
    )

    | SplAsNum (e1) -> (
        let ty1 = typeOf env e1 in
        match ty1 with
            SplatString -> SplatNumber
            | _ -> raise (TypeError ("Invalid type: NUM cannot operate on "^(type_to_string(ty1))))
    )

    | SplVariable (x) ->  (try lookup env x with
        (LookupError "Variable does not exist in environment") ->
                raise (TypeError "Expression is not a variable"))

    | _ -> raise (TypeError ("Unmatched function: Type checking does not exist
    for this function"))

let typeProg e = typeOf (Env []) e ;;

let rec list_to_string = function
    (SplString n) :: [] -> n
    | (SplNumber n) :: [] -> string_of_float n
    | (SplIfElse (a, b, c)) :: [] -> "ifelse"
    | (SplLet (a, b, c)) :: [] -> "let"
    | (SplAbs (a, b, c, d, e)) :: [] -> "function"
    | (SplApply (a, b)) :: [] -> "apply"
    | (SplShowLn n) :: xs -> "showln ; "^(list_to_string xs)
    | _ -> "_"
;;

let rec eval env e = match e with
  | (SplVariable x) -> (try ((lookup env x) , env) with
        (LookupError "Variable does not exist in environment") ->
            raise (UnboundVariableError "Variable does not exist in current
            environment"))

  | (SplNumber n) -> raise Terminated
  | (SplBoolean b) -> raise Terminated
  | (SplString s) -> raise Terminated
  | (SplList l) -> raise Terminated
  | (SplStream s) -> raise Terminated 
  | (SplAbs (rT, n, tT,x,e')) -> raise Terminated
  
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

  | (SplEq(n, m)) when (isValue(n) && isValue(m))   -> (SplBoolean( n = m ) , env)
  | (SplEq(n, m)) when (isValue(n))                 -> let (m',env') = (eval env m) in (SplEq(n, m'),env')
  | (SplEq(n, m))                                   -> let (n',env') = (eval env n) in (SplEq(n', m),env')
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
  | (SplIfElse(SplBoolean(n), e2, e3)) -> (
      if n then (eval_seq env e2) else (eval_seq env e3)
  )
  | (SplIfElse(e1, e2, e3))            -> let (e1',env') = (eval env e1) in (SplIfElse(e1', e2, e3) ,env')

  | (SplCons(n, SplList(m))) when (isValue(n))  -> (SplList( n :: m ), env)
  | (SplCons(n, e2)) when (isValue(n))          -> let (e2', env') = (eval env e2) in (SplCons(n, e2'), env')
  | (SplCons(n, e2))                            -> let (n', env') = (eval env n) in (SplCons(n', e2), env')

  | (SplHead(SplList(n))) -> (match n with
        h :: _ when (isValue h) -> (h, env)
        | [] -> (SplList(n), env)
        | _ -> raise (SyntaxError ("Cannot take HEAD of unrecognised expression
        "^Pervasives.__LOC__))
  )
  | (SplHead(e1)) -> let (e1', env') = (eval env e1) in (SplHead (e1'), env')

  | (SplTail(SplList(n))) -> (match n with
        _ :: t -> (SplList(t), env)
        | [] -> raise (OutOfBounds "At end of list, cannot take tail")
    )
  | (SplTail(e1)) -> let (e1', env') = (eval env e1) in (SplTail(e1'), env')
  
  | (SplStreamEnd (SplString(e1))) -> (SplBoolean ( e1 = "eof" ), env)
  | (SplStreamEnd (SplNumber(e1))) -> (SplBoolean ( e1 = Pervasives.nan ), env)
  | (SplStreamEnd (e1)) -> let (e1', env') = (eval env e1) in (SplStreamEnd
        (e1'), env')

  | (SplEmptyList (SplList(e1))) -> (SplBoolean ( e1 = [] ), env)
  | (SplEmptyList (e1)) -> let (e1', env') = (eval env e1) in 
        (SplEmptyList(e1'), env')

  (*Assignment*)
  | (SplLet(n, m, e3)) when (isValue(m)) -> 
          (eval_seq (addBinding env n m) e3)
  | (SplLet(n, m, e3)) -> let (m', env') = (eval env m) in (SplLet(n, m', e3), env')

  | (SplApply(SplAbs(rT,n,tT,x,e), e2)) when (isValue (e2)) -> (
      let env' = (addBinding 
        (addBinding env n (SplAbs(rT,n,tT,x,e))) x e2) in 
      (eval_seq env' e)
  )
  | (SplApply(SplAbs(rT, n, tT, x, e), e2)) ->
          let (e2', env') = (eval env e2) in 
          (SplApply(SplAbs(rT, n, tT, x, e), e2'), env)
  | (SplApply(e1,e2)) -> let (e1',env') = (eval env e1) in (SplApply(e1',e2), env')

  (*Predefined functions*)
  | (SplShow(SplNumber n)) -> ((let () =
      (print_int (int_of_float n); print_string " ") in SplNumber(n)), env)
  | (SplShow(SplBoolean n)) -> ((let () =
      (print_string (if n then "true " else "false ")) in
        SplBoolean(n)), env)
  | (SplShow(SplList(n))) -> ((let () =
      (print_list n; print_string " ") in (SplList n)), env)
  | (SplShow(SplString(n))) -> ((let () =
      (print_string n; print_string " ") in (SplString n)), env)
  | (SplShow(e1)) -> let (e1', env') = (eval env e1) in (SplShow(e1'), env')

  | (SplShowLn(SplNumber n)) -> ((let () =
      (print_int (int_of_float n); print_string "\n") in SplNumber(n)), env)
  | (SplShowLn(SplBoolean n)) -> ((let () =
      (print_string (if n then "true\n" else "false\n")) in
        SplBoolean(n)), env)
  | (SplShowLn(SplList(n))) -> ((let () =
      (print_list n; print_string "\n") in (SplList n)), env)
  | (SplShowLn(SplString(n))) -> ((let () =
      (print_string n; print_string "\n") in (SplString n)), env)
  | (SplShowLn(e1)) -> let (e1', env') = (eval env e1) in (SplShowLn(e1'),env)


  | (SplSplit(SplString(s)))    -> ((split s), env)
  | (SplSplit(s))               -> let (s', env') = (eval env s) in (SplSplit(s'), env')

  | (SplAsNum(SplString(s)))    -> (match (String.lowercase s) with 
        "eof" -> (SplNumber(Pervasives.nan), env)
        | _ -> (SplNumber (float_of_string s), env)
  )
  | (SplAsNum(s))               -> let (s', env') = (eval env s) in (SplAsNum(s'), env')

  | ex -> raise Terminated 


and eval_seq env n = match n with
    | expr :: [] -> 
        (if (isValue expr) then (expr, env) else (eval env expr)) 
    | expr :: expr_lst -> (
        let _ = (eval env expr) in ();
        (eval_seq env expr_lst)
    )
    | x -> raise (FunctionError "No function body")
;;

let rec evalloop env e = try (let (e',env') = (eval env e) in 
    (evalloop env' e')) with Terminated -> 
        if (isValue e) then e else e (*raise (StuckTerm e)*) ;;
let evalProg e = evalloop (Env []) e ;;

let rename (s:string) = s^"'";;


(*let rec spl_to_string expr = match expr with
    | (SplNumber n) -> string_of_float n
    | (SplBoolean n) -> string_of_bool n
    | (SplString n) -> n
    | (SplAbs (a, b, c, d, e)) -> 
            "Function: "^(type_to_string (typeProg expr))
    | (SplList n) ->  n
    | (SplPlus (a, b)) -> (spl_to_string a)^" + "^(spl_to_string b)
    (* TODO add all functions here *)
*)
let print_res res = match res with
    | (SplNumber i) -> print_int (int_of_float i) ; print_string " : Number"
    | (SplBoolean b) -> print_string (if b then "true" else "false") ; print_string " : Bool"
    | (SplString s) -> print_string (s^" : String")
    | (SplAbs(rT,n,tT,x,e)) -> print_string ("Function : "^type_to_string( typeProg (res) ))
    | (SplList l) -> print_list l; print_string (" :
        "^type_to_string ( typeProg res ))
    | (SplStream s) -> print_string "Some sort of stream here!"
    (*Comment up to raise error to stop debugging*)
    (* | (SplApply(e1, e2)) -> print_string "apply"
    | (SplLet(e1, e2, e3)) -> print_string "let" *)
    | _ -> raise (NonBaseTypeResult "Unrecognised base type")
