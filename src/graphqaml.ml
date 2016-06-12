open Lexer
open Lexing

include GQLTypes

let parseQuery q =    
    try Some (Parser.root_query Lexer.read (Lexing.from_string q)) with
    | SyntaxError msg ->
        print_string msg;
        None
    | Parser.Error ->
        print_string "Parser error\n";
        None

(*        
let _ =
    (match result with
    | Some q -> "Got a valid query, identifier: " ^ q.identifier    
    | None -> "Invalid query")
    |> print_string *)