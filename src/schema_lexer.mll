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

rule read =
    parse [' ' '\t' '\n'] { read lexbuf }
    | "query"    { QUERY }
    | "type"     { TYPE }
    | "enum"     { ENUM }
    | "schema"   { SCHEMA }
    | "mutation" { MUTATION }
    | '{'        { LEFT_BRACE }
    | '}'        { RIGHT_BRACE }
    | '['        { LEFT_SQUARE_BRACE }
    | ']'        { RIGHT_SQUARE_BRACE }
    | ':'        { COLON }
    | '!'        { EXCLAMATION }
    | ['a'-'z' 
       'A'-'Z' 
       '_']*     { IDENTIFIER (Lexing.lexeme lexbuf) }
    | eof        { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
