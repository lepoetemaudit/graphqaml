open OUnit2
open GraphQaml

 
let test_parser_empty_query test_ctxt =
  assert_equal 
    (GraphQaml.parseQuery "{ empty {} }")
    ( Some { GQLQuery.fields = []; identifier = "empty"; arguments = [] } )

let test_parser_single_field test_ctxt =
  let queryResult = (GraphQaml.parseQuery "{ single { jim } }") in
  assert_equal 
    queryResult
    ( Some { GQLQuery.fields = [ { GQLField.identifier = "jim"; fields = []}]; 
                        identifier = "single"; arguments = [] } )

let parseTests =
  "parser_tests">:::
  ["test_empty_query">:: test_parser_empty_query;
   "test_single_field">:: test_parser_single_field;]

let () =
  run_test_tt_main parseTests

