(* Schema Parser *)

%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token QUERY
%token TYPE
%token <string> IDENTIFIER

%{
open Schema

%}

%start <Schema.schema_item list> schema
%%

schema:
    | LEFT_BRACE; items = schema_items; RIGHT_BRACE; { items }
    ;

schema_items:
    | (* empty *) { [] }
    | QUERY; LEFT_BRACE; bob = IDENTIFIER; RIGHT_BRACE; si = schema_items; { Query { name = bob; } :: si }
    ;