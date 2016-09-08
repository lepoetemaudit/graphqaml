module Query = struct
  type t  = {
    name: string
  }
  [@@deriving show]
end

module Field = struct
  type t = {
      name: string;
      type_name: string;
      null: bool;
      list: bool;
  }
  [@@deriving show]
end

module Type = struct
  type t = {
    name: string;
    fields: Field.t list
  }
  [@@deriving show]

  let built_ins = ["int"; "string"; "float"; "boolean"; "id"]
end

module Enum = struct
  type t = {
    name: string;
    values: string list;
  }
  [@@deriving show]
end

module Schema = struct
  type t = (string * string) list
  [@@deriving show]
end

module SchemaItem = struct
  type t =
  | Query of Query.t
  | Type of Type.t
  | Enum of Enum.t
  | Schema of Schema.t
  | Empty
  [@@deriving show]
end

