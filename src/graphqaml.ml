open Lexing

include Query_types
include Schema_types

let parse_query q =
    let open Query_parser in
    let open Query_lexer in
    try Ok (root_query Query_lexer.read (Lexing.from_string q)) with
    | SyntaxError msg ->
        Error msg
    | Error ->
        Error "Undefined Parsing Error (sorry)\n"

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

let parse_schema q =
    let open Schema_parser in
    let open Schema_lexer in
    try Ok (schema read (Lexing.from_string q)) with
    | SyntaxError msg ->
        Error msg
    | Error ->
        Error "Undefined Parser Error (sorry)\n"