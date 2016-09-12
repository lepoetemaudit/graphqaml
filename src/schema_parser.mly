(* Schema Parser *)

%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PARENS
%token RIGHT_PARENS
%token LEFT_SQUARE_BRACE
%token RIGHT_SQUARE_BRACE
%token COLON
%token COMMA
%token EQUALS
%token QUERY
%token MUTATION
%token SCALAR
%token TYPE
%token SCHEMA
%token ENUM
%token EXCLAMATION
%token <string> STRING
%token EOF
%token <string> IDENTIFIER

%{ open Schema_types 
   exception SyntaxError of string
%}


%start <Schema_types.SchemaItem.t list> schema
%%

(* in certain contexts, a reserved word can be used as a normal identifer *)
general_ident:
    | ident = IDENTIFIER; { ident }
    | TYPE; { "type" }

schema:
    | items = schema_items; EOF; { items }   
    | ident = IDENTIFIER; { raise (SyntaxError ("Unknown identifier '" ^ ident ^ "'")) } 

schema_items:
    | (* empty *) { [] }
    | TYPE; t = type_; rs = schema_items; { t :: rs }
    | ENUM; name = IDENTIFIER; LEFT_BRACE; values = enum_values; RIGHT_BRACE; si = schema_items;
        { SchemaItem.Enum { Enum.name = name; Enum.values = values } :: si }
    | QUERY; LEFT_BRACE; name = IDENTIFIER; RIGHT_BRACE; si = schema_items; 
        { SchemaItem.Query { Query.name = name; } :: si }    
    | SCHEMA; LEFT_BRACE; so = schema_root_objs; RIGHT_BRACE; si = schema_items;
        { SchemaItem.Schema so :: si }
    | SCALAR; name = IDENTIFIER; si = schema_items;
        { SchemaItem.Scalar name :: si }

schema_root_objs:
    | (* empty *) { [] }
    | QUERY; COLON; type_ = IDENTIFIER; so = schema_root_objs;
        { Schema.Query type_ :: so }
    | MUTATION; COLON; type_ = IDENTIFIER; so = schema_root_objs;
        { Schema.Mutation type_ :: so } 

    
enum_values:
    | (*empty *) { [] }
    | ident = IDENTIFIER; values = enum_values; { ident :: values }

type_:
    | ident = IDENTIFIER; LEFT_BRACE; fields = type_field; RIGHT_BRACE; 
        { SchemaItem.Type { name = ident; Type.fields = fields } }

field_params:
    | (* empty *) { [] }
    | LEFT_PARENS; obj = separated_list(COMMA, argument); RIGHT_PARENS; { obj }

type_field:
    | (* empty *) { [] }
    | ident = IDENTIFIER; fp = field_params; COLON; lt = listable_type; rs = type_field;                 
        { 
            let (nt, is_list) = lt in
            let (type_name, is_null) = nt in
            { Field.null = is_null; name = ident; type_name = type_name; Field.list = is_list; Field.params = fp } :: rs }
    | ident = IDENTIFIER; { raise (SyntaxError ("Unexpected identifier in type field: " ^ ident)) }

argument:
    | name = general_ident; COLON; lt = listable_type; dv = default_value;
        {
            let (nt, is_list) = lt in
            let (type_name, is_null) = nt in
            { Param.name = name;
              Param.null = is_null;
              Param.type_name = type_name;
              Param.list = is_list;
              Param.default = dv; }
        }

default_value:
    | (* empty *) { Value.Nothing }
    | EQUALS; v = value; { v }

value:
    | v = STRING; { Value.String v }

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