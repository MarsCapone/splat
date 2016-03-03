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
    | splAdd of splatTerm * splatTerm 
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
    | splIf of splatTerm * splatTerm * splatTerm 
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


