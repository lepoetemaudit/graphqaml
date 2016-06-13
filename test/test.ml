open OUnit2
open Graphqaml


let test_parser_empty_query test_ctxt =
  assert_equal 
    (Graphqaml.parseQuery "{ empty {} }")
    ( Some { fields = []; identifier = "empty"; arguments = [] } )

let test_parser_single_field test_ctxt =
  let queryResult = (Graphqaml.parseQuery "{ single { jim } }") in
  assert_equal 
    queryResult
    ( Some { fields = [ { identifier = "jim"; fields = []}]; 
             identifier = "single"; arguments = [] } )

let parseTests =
  "parser_tests">:::
  ["test_empty_query">:: test_parser_empty_query;
   "test_single_field">:: test_parser_single_field;]

let () =
  run_test_tt_main parseTests

