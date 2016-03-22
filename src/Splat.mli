(* Types of the language *)
type splType =
    SplatNumber
    | SplatBoolean
    | SplatString
    | SplatList
    | SplatFunction of splType * splType

(* Grammar of the language *)
type splTerm =
    | SplNumber of float
    | SplBoolean of bool
    | SplString of string
    | SplList of splTerm list
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
    | SplJustDo of splTerm * splTerm
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
    | SplShowLn of splTerm
    | SplRange of splTerm * splTerm * splTerm
    | SplSplit of splTerm
    | SplAsNum of splTerm

(*Function stuff*)
    | SplApply of splTerm * splTerm
    | SplAbs of splType * string * splType * string * splTerm

val typeProg : splTerm -> splType
val evalProg :  splTerm -> splTerm

val print_res : splTerm -> unit
