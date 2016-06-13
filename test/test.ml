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

let test_parser_sub_fields test_ctxt =
  (match Graphqaml.parseQuery "{ nested { top { bottom } } }" with
   | Some _ -> true
   | None -> false)
  |> assert_equal true 

let parseTests =
  "parser_tests" >:::
  [ "test_empty_query" >:: test_parser_empty_query;
    "test_single_field" >:: test_parser_single_field;
    "test_parser_sub_fields" >:: test_parser_sub_fields; ]

let () =
  run_test_tt_main parseTests

