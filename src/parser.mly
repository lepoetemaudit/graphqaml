%token LEFT_BRACE
%token RIGHT_BRACE
%token <string> IDENTIFIER

%{
open Gqltypes
%}

%start <Gqltypes.gql_query> root_query
%%

root_query:
    | LEFT_BRACE; ident = IDENTIFIER; LEFT_BRACE; fs = fields;
       RIGHT_BRACE; RIGHT_BRACE;
        { { fields = fs ; arguments = []; identifier = ident; } }
    ;

fields:
    | ident = IDENTIFIER; LEFT_BRACE; subfields = fields; RIGHT_BRACE; fs = fields;
        { { fields = subfields; identifier = ident; } :: fs}
    | ident = IDENTIFIER;; fs = fields;
        { { fields = []; identifier = ident; } :: fs}
    | (* empty *) { [] }