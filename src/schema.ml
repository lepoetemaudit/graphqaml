type query = {
    name: string
}

type mutation = {
    name: string
}

type root_schema = {
    queries: query list;
    mutations: mutation list;
}

type t = {
    schema: root_schema
}

type field = {
    name: string;
    type_name: string;
    null: bool;
}

type type_ = {
    name: string;
    fields: field list
}

type schema_item = 
    | Query of query 
    | Type of type_
    | Empty