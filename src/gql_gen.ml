open Core.Std

let pp =
    [%show: Graphqaml.SchemaItem.t list]

let parse_file filename =
  match In_channel.read_all filename |> Graphqaml.parse_schema with
  | Error err -> print_string err
  | Ok res ->  print_string (pp res)
  

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () -> parse_file filename )

let () =
  Command.run ~version:"1.0" command