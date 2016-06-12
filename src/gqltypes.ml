type argument_value =
  | String of string
  | Int of int
  | Bool of bool

type argument =
  { identifier : string;
    value : argument_value;
  }

module GQLField = struct
  type t = 
    { identifier : string;
      fields : t list }
end


module GQLQuery = struct
  type t =
      { identifier : string; 
          fields : GQLField.t list;
          arguments : argument list;
      }
end