{
open Lexing
open Schema_parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
    parse
    | white      { read lexbuf }
    | newline    { next_line lexbuf; read lexbuf }
    | "query"    { QUERY }
    | "type"     { TYPE }
    | "enum"     { ENUM }
    | "schema"   { SCHEMA }
    | "mutation" { MUTATION }
    | "scalar"   { SCALAR }
    | '"'        { read_string (Buffer.create 17) lexbuf }
    | '('        { LEFT_PARENS }
    | ')'        { RIGHT_PARENS }
    | ','        { COMMA }
    | '{'        { LEFT_BRACE }
    | '}'        { RIGHT_BRACE }
    | '['        { LEFT_SQUARE_BRACE }
    | ']'        { RIGHT_SQUARE_BRACE }
    | ':'        { COLON }
    | '!'        { EXCLAMATION }
    | '='        { EQUALS }
    | ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']*     { IDENTIFIER (Lexing.lexeme lexbuf) }
    | eof        { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf =
    parse
    | '"'       { STRING (Buffer.contents buf) }
    | [^ '"' '\\']+
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
        }
    | eof  { raise (SyntaxError ("String is not terminated")) }

    