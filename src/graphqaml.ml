open Lexer
open Lexing

include Gqltypes

let parse_query q =    
    try Ok (Parser.root_query Lexer.read (Lexing.from_string q)) with
    | SyntaxError msg ->
        Error msg;
    | Parser.Error ->
        Error "Undefined Parser Error (sorry)\n";