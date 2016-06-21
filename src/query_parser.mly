%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token <string> IDENTIFIER

%{
open Query_types
%}

%start <Query_types.gql_query> root_query
%%

root_query:
    | LEFT_BRACE; ident = IDENTIFIER; LEFT_BRACE; fs = fields;
       RIGHT_BRACE; RIGHT_BRACE;
        { { fields = fs ; arguments = []; identifier = ident; } }
    | LEFT_BRACE; ident = IDENTIFIER; RIGHT_BRACE; { { fields = []; arguments = []; identifier = ident; }}
    

fields:        
    | ref = field_ref; LEFT_BRACE; subfields = fields; RIGHT_BRACE; fs = fields;
        { { fields = subfields; identifier = fst ref; alias = snd ref } :: fs}
    | ref = field_ref; fs = fields;
        { { fields = []; identifier = fst ref; alias = snd ref; } :: fs}    
    | (* empty *) { [] }

field_ref:
    | ident = IDENTIFIER; { ( ident, None ) };
    | alias = IDENTIFIER; COLON; ident = IDENTIFIER { ( ident, Some alias; ) }