(* Schema Parser *)

%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_SQUARE_BRACE
%token RIGHT_SQUARE_BRACE
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
    | ident = IDENTIFIER; COLON; lt = listable_type; rs = type_field;                 
        { 
            let (nt, is_list) = lt in
            let (type_name, is_null) = nt in
            { Field.null = is_null; name = ident; type_name = type_name; Field.list = is_list; } :: rs }

nullable_type:
    | kind = IDENTIFIER; EXCLAMATION;
        { kind, false }
    | kind = IDENTIFIER;
        { kind, true }

listable_type:
    | LEFT_SQUARE_BRACE; nt = nullable_type; RIGHT_SQUARE_BRACE;
        { ( nt, true) }
    | nt = nullable_type;
        { ( nt, false ) }