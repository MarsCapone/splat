type splatType =
  SplatNumber
| SplatBoolean
| SplatString
| SplatList
| SplatStream
| SplatFunction of splatType * splatType;;

type toyTerm =
    TmNum of float
    | TmBool of bool
    | TmLessThan of toyTerm * toyTerm
    | TmPlus of toyTerm * toyTerm
    | TmVar of string
    | TmIf of toyTerm * toyTerm * toyTerm
    | TmLet of string * splatType * toyTerm * toyTerm
    | TmAbs of string * splatType * toyTerm
    | TmApp of toyTerm * toyTerm

val typeProg : toyTerm -> splatType
val evalProg :  toyTerm -> toyTerm

val print_res : toyTerm -> unit
