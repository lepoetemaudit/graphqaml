open OUnit2
open Graphqaml

let test_parser_empty_query test_ctxt =
  assert_equal 
    (Graphqaml.parse_query "{ empty {} }")
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

(* Dummy schema test *)
let test_schema test_ctxt =
  assert_equal 
    (Graphqaml.parse_schema "{ type bob { jim: int }}")
    (Ok [ Type { name = "bob"; fields = [ { name = "jim"; type_name = "int"; null = true }]} ]);

  assert_equal 
    (Graphqaml.parse_schema "{ type bob { jim: int! }}")
    (Ok [ Type { name = "bob"; fields = [ { name = "jim"; type_name = "int"; null = false }]} ])   
  

let test_suite =
  "graphqaml_tests" >:::
  [ "test_empty_query" >:: test_parser_empty_query;
    "test_single_field" >:: test_parser_single_field;
    "test_parser_sub_fields" >:: test_parser_sub_fields;
    "test_error_message" >:: test_error_message; 
    "test_alias" >:: test_alias; 

    "test_serialisation" >:: test_serialisation;

    "test_schema" >:: test_schema;
  ]


let () =
  run_test_tt_main test_suite


