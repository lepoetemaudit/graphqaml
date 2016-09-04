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

module Field = struct
    type t = {
        name: string;
        type_name: string;
        null: bool;
    }
end



module Type = struct
    type t = {
        name: string;
        fields: Field.t list
    }

    let built_ins = ["int"; "string"; "float"; "boolean"; "id"]
end


module SchemaItem = struct
    type t = 
        | Query of query 
        | Type of Type.t
        | Empty
end