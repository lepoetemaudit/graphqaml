module Query = struct
  type t  = {
    name: string
  }
  [@@deriving show]
end

module Value = struct
  type t =
  | Int of int
  | Bool of bool
  | String of string
  | Nothing
  [@@deriving show]
end

module Param = struct
  type t = {
    name: string;
    type_name: string;
    null: bool;
    list: bool;
    default: Value.t;
  }
  [@@deriving show]
end

module Field = struct
  type t = {
      name: string;
      type_name: string;
      null: bool;
      list: bool;
      params: Param.t list; 
  }
  [@@deriving show]
end

module Type = struct
  type t = {
    name: string;
    fields: Field.t list
  }
  [@@deriving show]

  let built_ins = ["Int"; "String"; "Float"; "Boolean"; "ID"]
end

module Enum = struct
  type t = {
    name: string;
    values: string list;
  }
  [@@deriving show]
end

module Schema = struct
  type root_type = | Query of string | Mutation of string  
  and t = root_type list
  [@@deriving show]
end

module Scalar = struct
  type t = string
  [@@deriving show]
end

module SchemaItem = struct
  type t =
  | Query of Query.t
  | Type of Type.t
  | Enum of Enum.t
  | Schema of Schema.t
  | Scalar of Scalar.t
  | Empty
  [@@deriving show]
end

module Root = struct
  type t = {
    query: Query.t;
    migration: Query.t
  }
  [@@deriving show]
end
