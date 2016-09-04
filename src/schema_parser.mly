(* Schema Parser *)

%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token QUERY
%token TYPE
%token EXCLAMATION
%token <string> IDENTIFIER

%{ open Schema_types %}

%start <Schema_types.SchemaItem.t list> schema
%%

schema:
    | LEFT_BRACE; items = schema_items; RIGHT_BRACE; { items }    

schema_items:
    | (* empty *) { [] }
    | TYPE; t = type_; rs = schema_items; { t :: rs }
    | QUERY; LEFT_BRACE; bob = IDENTIFIER; RIGHT_BRACE; si = schema_items; 
        { SchemaItem.Query { name = bob; } :: si }
    

type_:
    | ident = IDENTIFIER; LEFT_BRACE; fields = type_field; RIGHT_BRACE; 
        { SchemaItem.Type { name = ident; Type.fields = fields } }

type_field:
    | (* empty *) { [] }
    | ident = IDENTIFIER; COLON; kind = IDENTIFIER; rs = type_field; 
        { { Field.null = true; name = ident; type_name = kind; } :: rs }

    | ident = IDENTIFIER; COLON; kind = IDENTIFIER; EXCLAMATION; rs = type_field; 
        { { Field.null = false; name = ident; type_name = kind; } :: rs }