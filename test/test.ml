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
    

let parseTests =
  "parser_tests" >:::
  [ "test_empty_query" >:: test_parser_empty_query;
    "test_single_field" >:: test_parser_single_field;
    "test_parser_sub_fields" >:: test_parser_sub_fields;
    "test_error_message" >:: test_error_message; 
    "test_alias" >:: test_alias; ]

let () =
  run_test_tt_main parseTests

