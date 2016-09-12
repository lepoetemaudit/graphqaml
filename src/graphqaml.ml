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

let _validate_schema schema =
    (* Discover all newly declared types *)
    let module SI = SchemaItem in    
    let types = List.filter_map schema 
        (function | SI.Type type_ -> Some type_ 
                  | _ -> None) in    

    let enums = List.filter_map schema
        (function | SI.Enum enum -> Some enum
                  | _ -> None) in

    (* Extract the names of new types (and builtins) into a set *)
    let registered_types = types 
                           |> List.map ~f:(fun t -> t.Type.name)
                           |> List.append (List.map ~f:(fun e -> e.Enum.name) enums) 
                           |> List.append Type.built_ins
                           |> String.Set.of_list in

    (* Extract all referenced type names into a set *)
    let type_refs = types
                    |> List.map ~f:(fun t -> t.Type.fields) 
                    |> List.concat
                    |> List.map ~f:(fun f -> f.Field.type_name)
                    |> String.Set.of_list
                    in

    (* Discover any types referenced but not declared *)
    match String.Set.diff type_refs registered_types |> String.Set.to_list with
    | [] -> Ok schema
    | failing_types -> 
        Error ("Failing types: " ^ (String.concat failing_types ~sep: ", "))

let _parse_schema q =
    let open Schema_parser in
    let open Schema_lexer in
    let open Lexing in
    let filebuf = Lexing.from_string q in
    (try Ok (schema read filebuf) with
    | SyntaxError msg ->
        (Result.Error msg)
    | Error ->
        let start = Lexing.lexeme_start_p filebuf in
        let tok = Lexing.lexeme filebuf in
        (Result.Error (Printf.sprintf "Line %d, character %d: Unexpected token '%s'\n" 
                       start.pos_lnum
                       (start.pos_cnum - start.pos_bol)               
                       tok)))
    

let parse_schema q =
    _parse_schema q >>= _validate_schema