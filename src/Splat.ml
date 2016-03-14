exception LookupError ;;
exception TypeError ;;
exception UnboundVariableError;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;

(* Splat Grammar *)

type splatType =
    | splatNumber
    | splatBoolean
    | splatString
    | splatList
    | splatStream
    | splatFunction of splatType * splatType;;

type splatTerm =
    | splNumber of float
    | splBoolean of bool
    | splString of string
    | splList of list
    | splStream of Stream
    | splVariable of string
(* number operators *)
    | splPlus of splatTerm * splatTerm
    | splMinus of splatTerm * splatTerm
    | splTimes of splatTerm * splatTerm
    | splDivide of splatTerm * splatTerm
    | splModulo of splatTerm * splatTerm
    | splPower of splatTerm * splatTerm
(* boolean operators *)
    | splNot of splatTerm
    | splAnd of splatTerm * splatTerm
    | splOr of splatTerm * splatTerm
(* flow *)
    | splFor of splatTerm * splatTerm * splatTerm
    | splForever of splatTerm
    | splWhile of splatTerm * splatTerm
    | splIfElse of splatTerm * splatTerm * splatTerm
    | splIf of splatTerm * splatTerm
    | splSwitch of splatTerm * splatTerm
(* comparators *)
    | splLt of splatTerm * splatTerm
    | splGt of splatTerm * splatTerm
    | splLe of splatTerm * splatTerm
    | splGe of splatTerm * splatTerm
    | splEq of splatTerm * splatTerm
    | splNe of splatTerm * splatTerm
(* assignment *)
    | splAssign of string * splatTerm
    | splPlusAssign of string * splatTerm
    | splMinusAssign of string * splatTerm
    | splTimesAssign of string * splatTerm
    | splDivideAssign of string * splatTerm
(* predefined functions *)
    | splShow of splatTerm
    | splRange of splatTerm * splatTerm * splatTerm
    | splSplit of splatTerm
;;

let rec isValue e = match e with
    | splNumber (n) -> true
    | splBoolean (n) -> true
    | splString (n) -> true
    | splStream (n) -> true
    | splList (n) -> true
    | _ -> false
;;


(* Type of Environments *)
type 'a context = Env of (string * 'a) list
type typeContext = splatType context
type valContext = splatTerm context

(* Lookup type of a string name variable in a type enviroment *)
let rec lookup env str = match env with
    Env [] -> raise LookupError
    | Env ((name, thing) :: gs) ->
        ( match (name = str) with
            true -> thing
            | false -> lookup (Env (gs)) str
        )
;;

(* Add entry in environemnt *)
let addBinding env str thing = match env with
    Env (gs) -> Env ((str, thing) :: gs)
;;

(* Type checking function *)
let rec typeOf env e = match e with
    splNumber (n) -> splatNumber
    | splBoolean (n) -> splatBoolean
    | splString (n) -> splatString
    | splList (n) -> splatList
    | splStream (n) -> splatStream
    | splVariable (n) -> (try lookup env n with LookupError -> TypeError)

    (* Mathematical operators *)
    | splPlus (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatNumber
            | _ -> raise TypeError
        )

    | splMinus (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatNumber
            | _ -> raise TypeError
    )

    | splTimes (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatNumber
            | _ -> raise TypeError
    )

    | splDivide (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatNumber
            | _ -> raise TypeError
    )

    | splModulo (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatNumber
            | _ -> raise TypeError
    )

    | splPower (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatNumber
            | _ -> raise TypeError
        )

    (* Boolean Operators *)
    | splNot (n) -> (
        match (typeOf env n) with
            splatBoolean -> splatBoolean
            | _ -> TypeError
        )

    | splAnd (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatBoolean, splatBoolean -> splatBoolean
            | _ -> raise TypeError
    )

    | splOr (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatBoolean, splatBoolean -> splatBoolean
            | _ -> raise TypeError
        )

    (* Flow *)
    | splFor (a, b, c) -> (typeOf env c)

    | splForever (n) -> (typeOf env n)

    | splWhile (a, b) -> (typeOf env b)

    | splIfElse (a, b, c) -> (
        let s1 = typeOf env a in
            match s1 with
                splatBoolean -> (
                    let s1 = typeOf env b in
                        let s2 = typeOf env c in
                            ( match (s1 = s2) with
                                true -> s1
                                | false -> raise TypeError
                            )
                    )
                | _ -> raise TypeError
        )

    | splIf (a, b) -> (typeOf env b)


    (* Comparators *)
    | splLt (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatBoolean
            | _ -> raise TypeError
        )

    | splGt (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatBoolean
            | _ -> raise TypeError
        )

    | splLe (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatBoolean
            | _ -> raise TypeError
        )

    | splGe (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatBoolean
            | _ -> raise TypeError
        )

    | splEq (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatBoolean
            | _ -> raise TypeError
        )

    | splNe (a, b) -> (
        match (typeOf env a), (typeOf env b) with
            splatNumber, splatNumber -> splatBoolean
            | _ -> raise TypeError
        )
;;


let typeProg e = typeOf (Env []) e ;;

let rec eval env e = match e with
    | (splatVariable x) -> (try ((lookup env x), env) with LookupError -> raise UnboundVariableError)
    | (splNumber n) -> raise Terminated
    | (splBoolean n) -> raise Terminated
    | (splString n) -> raise Terminated
    | (splStream n) -> raise Terminated
    | (splList n) -> raise Terminated

    | (splLt (splNumber(a), splNumber(b))) -> (splBoolean( a < b ), env)
    | (splGt (splNumber(a), splNumber(b))) -> (splBoolean( a > b ), env)
    | (splLe (splNumber(a), splNumber(b))) -> (splBoolean( a <= b ), env)
    | (splGe (splNumber(a), splNumber(b))) -> (splBoolean( a >= b ), env)
    | (splEq (splNumber(a), splNumber(b))) -> (splBoolean( a == b ), env)
    | (splNe (splNumber(a), splNumber(b))) -> (splBoolean( a != b ), env)

    | _ -> raise Terminated;;

let rec evalloop env e = try (let (e',env') = (eval env e) in (evalloop env' e')) with Terminated -> if (isValue e) then e else raise StuckTerm  ;;
let evalProg e = evalloop (Env []) e ;;
