open OUnit2
open Graphqaml
open Core.Std
open Result

(* Wrap Result.t for @@deriving show *)
module MRes = struct
  type ('ok, 'err) t = ('ok, 'err) Result.t = Ok of 'ok |	Error of 'err
  [@@deriving show]
end

(* Generate a pretty printer for schema results *)
let schema_result_printer = 
  [%show: ((Graphqaml.SchemaItem.t list), string) MRes.t]

let test_parser_empty_query test_ctxt =
  assert_equal
    (Graphqaml.parse_query "{ empty }")
    ( Ok { fields = []; identifier = "empty"; arguments = [] } )

(* Test that field name appears *)
let test_parser_single_field test_ctxt =
  let queryResult = (Graphqaml.parse_query "{ single { jim } }") in
  assert_equal 
    queryResult
    ( Ok { fields = [ { identifier = "jim"; fields = []; alias = None }]; 
           identifier = "single"; arguments = [] } )

(* Test nested queries *)
let test_parser_sub_fields test_ctxt =
  match Graphqaml.parse_query "{ nested { top { bottom } } }" with
  | Ok _ -> ()
  | Error msg -> failwith ("Failed to parse: " ^ msg)

(* Test obvious bad grammar - non-matching parens *)
let test_error_message test_ctxt =
  (match Graphqaml.parse_query "{ mismatch { top { bottom } }" with
   | Error msg -> ()
   | Ok _ -> failwith "Should not have received value")

(* Test aliases *)
let test_alias test_ctxt = 
  let query_result = Graphqaml.parse_query "{ query { name: jim } }" in
  assert_equal
    query_result
    ( Ok { arguments = [];
           identifier = "query";
           fields = 
             [ { identifier = "jim"; 
                 fields = [];
                 alias = Some "name";                 
               } ] } )

(* Test serialisation (round trip) *)
let test_serialisation test_ctxt =
  let query_text = "{query{name age location {town country}}}" in
  let query_ast = match Graphqaml.parse_query query_text with
                  | Ok obj -> obj 
                  | _ -> failwith "Bad query" in
  let serialized_result = (Graphqaml.query_to_string query_ast) in
  assert_equal ~msg: (Printf.sprintf "Expected %s, got %s" 
                      query_text serialized_result)
               serialized_result query_text

(* Test schema type parsing *)
let test_builtin_schema_types test_ctxt =
  (match Graphqaml.parse_schema "{ type bob { jim: int }}" with
  | Error err -> failwith err;
  | Ok _ -> ())

(* Test references to new types *)
let test_schema_new_types test_ctxt =
  (match Graphqaml.parse_schema ("{ type person { name: int } " ^
                                 "  type employee { person: person } }") with
  | Error err -> failwith err;
  | Ok _ -> ())

let test_nullable_fields test_ctxt =
  let module SI = Graphqaml.SchemaItem in
  let module T = Graphqaml.Type in
  let module F = Graphqaml.Field in
  assert_equal 
    (Graphqaml.parse_schema "{ type bob { jim: int }}")
    (Ok [ SI.Type { T.name = "bob"
                  ; T.fields = [ { F.name = "jim" 
                                 ; F.type_name = "int"
                                 ; F.null = true
                                 ; F.list = false }]} ]);

  assert_equal 
    (Graphqaml.parse_schema "{ type bob { jim: int! }}")
       (Ok [ SI.Type { T.name = "bob"
                     ; T.fields = [ { F.name = "jim" 
                                  ; F.type_name = "int"
                                  ; F.null = false
                                  ; F.list = false }]} ]);;
  
let test_list_fields test_ctxt =
  let module SI = Graphqaml.SchemaItem in
  let module T = Graphqaml.Type in
  let module F = Graphqaml.Field in
  let v_with_list = Graphqaml.parse_schema "{ type bob { jim: [int] }}" in
  let v_no_list = Graphqaml.parse_schema "{ type bob { jim: int }}" in
  assert_equal 
    ~printer:schema_result_printer
    v_with_list
    (Ok [ SI.Type { T.name = "bob"
                  ; T.fields = [ { F.name = "jim" 
                                 ; F.type_name = "int"
                                 ; F.null = true
                                 ; F.list = true }]} ]);

  assert_equal 
    ~printer:schema_result_printer
    v_no_list
    (Ok [ SI.Type { T.name = "bob"
                  ; T.fields = [ { F.name = "jim" 
                                 ; F.type_name = "int"
                                 ; F.null = true
                                 ; F.list = false }]} ]);;                                 

let test_enums test_ctxt =
  let module SI = Graphqaml.SchemaItem in
  let module E = Graphqaml.Enum in
  let module F = Graphqaml.Field in
  let enums = Graphqaml.parse_schema "{ enum HUMAN { TED DOUGAL JACK MRS_DOYLE }}" in
  assert_equal 
    ~printer:schema_result_printer
    enums
    (Ok [ SI.Enum { E.name = "HUMAN"
                  ; E.values = [ "TED"; "DOUGAL"; "JACK"; "MRS_DOYLE" ] } ] );;

let test_suite =
  "graphqaml_tests" >:::
  [ (* Query parsing tests *)
    "test_empty_query"          >:: test_parser_empty_query;
    "test_single_field"         >:: test_parser_single_field;
    "test_parser_sub_fields"    >:: test_parser_sub_fields;
    "test_error_message"        >:: test_error_message; 
    "test_alias"                >:: test_alias; 

    (* Query serialisatoin tests *)
    "test_serialisation"        >:: test_serialisation;

    (* Schema parsing tests *)
    "test_builtin_schema_types" >:: test_builtin_schema_types;
    "test_schema_new_types"     >:: test_schema_new_types;
    "test_nullable_fields"      >:: test_nullable_fields; 
    "test_list_fields"          >:: test_list_fields;
    "test_enums"                >:: test_enums;
  ]

let () =
  run_test_tt_main test_suite
