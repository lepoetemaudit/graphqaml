type argument_value =
  | String of string
  | Int    of int
  | Bool   of bool

type argument =
  { identifier : string;
    value      : argument_value;
  }

type gql_field =
  { identifier : string;
    alias      : string option;
    fields     : gql_field list }


type gql_query =
  { identifier : string; 
    fields     : gql_field list;
    arguments  : argument list;
  }
