open Splat
open Lexer
open Parser
open Arg
open Printf

let parseProgram c =
    let lexbuf = Lexing.from_channel c in
        try 
            parser_main lexer_main lexbuf
        with Parsing.Parse_error -> 
            raise (Failure "Parse Error");;

(* Parsing.set_trace true; *)
let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./main PROGRAM_FILE" in
parse [] setProg usage ;
let parsedProg = parseProgram !arg in
let result1 = evalProg parsedProg in
let () = print_string "Returned " ; print_res result1 ; print_newline() in
flush stdout

