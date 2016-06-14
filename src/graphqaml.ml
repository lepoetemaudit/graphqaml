open Lexer
open Lexing

include Gqltypes

let parse_query q =    
    try Ok (Parser.root_query Lexer.read (Lexing.from_string q)) with
    | SyntaxError msg ->
        Error msg
    | Parser.Error ->
        Error "Undefined Parser Error (sorry)\n"

let rec field_to_string field =
    (match field.alias with
    | Some alias -> alias ^ ":"
    | None -> "") ^
    field.identifier ^ 
    (if List.length field.fields > 0 then 
        " {" ^ ((List.map field_to_string field.fields) |> String.concat " ") ^ "}"
    else "")

let query_to_string ast =
    (* being with root query *)
    "{" ^ (ast.identifier) ^ "{" ^ 
    ((List.map field_to_string ast.fields) |> String.concat " ")
    ^ "}}"