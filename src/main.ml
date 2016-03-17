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
            begin
                let curr = lexbuf.lex_curr_p in
                let line = string_of_int curr.pos_lnum in
                let cnum = string_of_int curr.pos_cnum in
                let bol = string_of_int curr.pos_bol in
                    failwith ("Parse error: Line:"
                        ^line^", CNum:"
                        ^cnum^", Bol:"^bol)
            end;;

(* Parsing.set_trace true; *)
let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./main PROGRAM_FILE" in
parse [] setProg usage ;
let parsedProg = parseProgram !arg in
let result1 = evalProg parsedProg in
let () = print_string "Returned " ; print_res result1 ; print_newline() in
flush stdout
