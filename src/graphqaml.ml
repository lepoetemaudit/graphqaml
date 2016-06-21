open Core.Std
open Result

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
        " {" ^ ((List.map field.fields field_to_string)
        |> String.concat ~sep:" " 
        ) ^ "}"
    else "")

let query_to_string ast =
    (* being with root query *)
    "{" ^ (ast.identifier) ^ "{" ^ 
        ((List.map ast.fields field_to_string) 
        |> String.concat ~sep:" ")
    ^ "}}"

exception BadType of string;;

let _check_type t =
    Some t.name

let _validate_schema schema =
    (* check all types *)
    let types = List.filter_map schema 
        (function | Type type_ -> Some type_ 
                  | _ -> None) in
    let bad_types = List.filter_map types _check_type in 
    
    match List.hd bad_types with
    | Some bad_type -> Error bad_type 
    | None -> Ok schema

let _parse_schema q =
    let open Schema_parser in
    let open Schema_lexer in
    (try Ok (schema read (Lexing.from_string q)) with
    | SyntaxError msg ->
        (Result.Error msg)
    | Error ->
        (Result.Error "Undefined Parser Error (sorry)\n"))
    

let parse_schema q =
    _parse_schema q >>= _validate_schema

    
  