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

type type_ = {
    name: string
}

type schema_item = 
    | Query of query 
    | Type of type_
    | Empty