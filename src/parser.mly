%token LEFT_BRACE
%token RIGHT_BRACE
%token <string> IDENTIFIER


%start <GQLTypes.GQLQuery.t> root_query
%%

root_query:
    | LEFT_BRACE; ident = IDENTIFIER; LEFT_BRACE; fs = fields;
       RIGHT_BRACE; RIGHT_BRACE;
        { { fields = fs ; arguments = []; GQLTypes.GQLQuery.identifier = ident; } }
    ;

fields:
    | ident = IDENTIFIER; LEFT_BRACE; RIGHT_BRACE; fs = fields;
        { { fields = []; GQLTypes.GQLField.identifier = ident; } :: fs}
    | ident = IDENTIFIER;; fs = fields;
        { { fields = []; GQLTypes.GQLField.identifier = ident; } :: fs}
    | (* empty *) { [] }